############
## Libraries
############

library(tidyverse)
library(DataExplorer) # corrrelation plot and NAs plot
library(GGally) # ggpairs
library(caret)
library(tictoc)

######################
## Data Initialization
######################

test <- read.csv("./test.csv")
train <- read.csv("./training.csv")

test <- test %>% 
        mutate(IsBadBuy = NA)

data_full <- bind_rows(test, train)

plot_missing(data_full)

# Change all character variables to factor variables
data_full <- data_full %>% 
             mutate(across(where(is_character),as_factor))

data_full$IsOnlineSale <- as.factor(data_full$IsOnlineSale)

# Change certain factor variables to numeric variables
data_full$MMRAcquisitionAuctionAveragePrice <- data_full$MMRAcquisitionAuctionAveragePrice %>% 
                                               as.character() %>% 
                                               as.numeric()
data_full$MMRAcquisitionAuctionCleanPrice <- data_full$MMRAcquisitionAuctionCleanPrice %>% 
                                             as.character() %>% 
                                             as.numeric()
data_full$MMRAcquisitionRetailAveragePrice <- data_full$MMRAcquisitionRetailAveragePrice %>% 
                                              as.character() %>% 
                                              as.numeric()
data_full$MMRAcquisitonRetailCleanPrice <- data_full$MMRAcquisitonRetailCleanPrice %>% 
                                           as.character() %>% 
                                           as.numeric()

data_full$MMRCurrentAuctionAveragePrice <- data_full$MMRCurrentAuctionAveragePrice %>% 
                                           as.character() %>% 
                                           as.numeric()
data_full$MMRCurrentAuctionCleanPrice <- data_full$MMRCurrentAuctionCleanPrice %>% 
                                         as.character() %>% 
                                         as.numeric()
data_full$MMRCurrentRetailAveragePrice <- data_full$MMRCurrentRetailAveragePrice %>% 
                                          as.character() %>% 
                                          as.numeric()
data_full$MMRCurrentRetailCleanPrice <- data_full$MMRCurrentRetailCleanPrice %>% 
                                        as.character() %>% 
                                        as.numeric()

# Check NA's now
plot_missing(data_full)

# Check correlations
plot_correlation(data_full)
ggpairs(data_full) # too many columns

###############################
## Select Features for Analysis
###############################

selected_features <- data_full %>% 
                     select(RefId,
                            Auction, 
                            VehYear, 
                            VehicleAge, 
                            Make, 
                            VehOdo, 
                            Nationality, 
                            MMRAcquisitionAuctionAveragePrice,
                            MMRAcquisitionAuctionCleanPrice,
                            MMRAcquisitionRetailAveragePrice,
                            MMRAcquisitonRetailCleanPrice,
                            MMRCurrentAuctionAveragePrice,
                            MMRCurrentAuctionCleanPrice,
                            MMRCurrentRetailAveragePrice,
                            MMRCurrentRetailCleanPrice,
                            VehBCost,
                            IsOnlineSale,
                            WarrantyCost,
                            IsBadBuy)

selected_features$IsBadBuy <- as.factor(selected_features$IsBadBuy)
levels(selected_features$IsBadBuy) <- c("No", "Yes")
levels(selected_features$IsOnlineSale) <- c("No", "Yes")

################
## Separate Data
################

# Separate test and training data
selected_features_train <- selected_features %>% 
                           filter(!is.na(IsBadBuy))

selected_features_test <- selected_features %>% 
                          filter(is.na(IsBadBuy)) %>% 
                          select(-IsBadBuy)

indexes <- createDataPartition(selected_features_train$IsBadBuy,
                               times = 1,
                               p = .8,
                               list = FALSE)

new_train <- selected_features_train[indexes,]
new_test <- selected_features_train[-indexes,]

rf_preprocess <- preProcess(new_train, method = "bagImpute")

ready_train <- predict(rf_preprocess, new_train)

rf_train_control <- trainControl(method = "repeatedcv",
                                 number = 4,
                                 repeats = 2,
                                 search = "grid",
                                 summaryFunction = prSummary,
                                 classProbs = TRUE)

rf_tune_grid <- expand.grid(mtry = c(4,5))

# Try a 'rf' model with the metric being "AUC"

tic()

rf_model <- train(as.factor(IsBadBuy) ~ .,
                  data = ready_train %>% select(-RefId),
                  method = "rf",
                  tuneGrid = rf_tune_grid,
                  trControl = rf_train_control,
                  metric = "AUC")

toc()

rf_preds <- predict(rf_model, new_test)

# Use the model to predict Real Test Set

rf_preprocess <- preProcess(selected_features_test, 
                            method = "bagImpute")

final_test <- predict(rf_preprocess, selected_features_test)

test_preds <- predict(rf_model, newdata = final_test)

pred_frame <- data.frame(RefId = final_test$RefId,
                         IsBadBuy = test_preds)

levels(pred_frame$IsBadBuy) <- c(0, 1)

write.csv(x = pred_frame,
          file = "./Car_Auction_Preds.csv", 
          row.names = FALSE)

# Try a 'rf' model with the metric being "F"

tic()

rf_model_1 <- train(as.factor(IsBadBuy) ~ .,
                  data = ready_train %>% select(-RefId),
                  method = "rf",
                  tuneGrid = rf_tune_grid,
                  trControl = rf_train_control,
                  metric = "F")

toc()
