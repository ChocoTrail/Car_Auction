############
## Libraries
############

test <- read.csv("./test.csv")
train <- read.csv("./training.csv")

test <- test %>% mutate(IsBadBuy = NA)

data_full <- bind_rows(test, train)

DataExplorer::plot_missing(data_full)

for (i in 1:length(to_factor_cols)) {
  data_full[,to_factor_cols[i]] <- as.factor(data_full[,to_factor_cols[i]])
}

data_full <- data_full %>% mutate(across(where(is_character),as_factor))

data_full$RefId <- as.factor(data_full$RefId)
data_full$MMRAcquisitionAuctionAveragePrice <- as.factor(data_full$MMRAcquisitionAuctionAveragePrice)
data_full$RefId <- as.factor(data_full$RefId)
data_full$RefId <- as.factor(data_full$RefId)
