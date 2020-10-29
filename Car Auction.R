############
## Libraries
############

library(tidyverse)
library(DataExplorer) # corrrelation plot and NAs plot
library(GGally) # ggpairs
library(caret)

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
                     select(Auction, 
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

################
## Separate Data
################

# Separate test and training data
selected_features_train <- selected_features %>% 
                           filter(!is.na(IsBadBuy))

selected_features_test <- selected_features %>% 
                           filter(is.na(IsBadBuy))

indexes <- createDataPartition(selected_features_train$IsBadBuy,
                               times = 1,
                               p = .8,
                               list = FALSE)

rf_train <- selected_features_train[indexes,]
rf_test <- selected_features_train[-indexes,]

# start here
rf_train_control <- trainControl(method = "repeatedcv",
                                 number = 4,
                                 repeats = 2,
                                 search = "grid",
                                 summaryFunction = prSummary)