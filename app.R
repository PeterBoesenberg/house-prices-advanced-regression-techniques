library(plotly)
library(data.table)
library(modules)
library(Metrics)
library(caret)

load <- modules::use("load.R")
clean <- modules::use("clean.R")
save <- modules::use("save.R")
prediction <- modules::use("predict.R")

is_kaggle_mode <- TRUE


if(is_kaggle_mode) {
  # prepare data variables for submission
  train <- load$load_train()
  train <- clean$clean(train)
  test_data <- load$load_test()
  test <- test_data
  test <- test[, SalePrice:=0]
  test <- clean$clean(test)
  
  result <- prediction$get_result(train, test, test_data)
  save$save(result)
} else {
  rmse_values <- c()
  
  for(i in 1:1) {
    # prepare data variables for local test, use test-train-split
    data <- as.data.table(load$load_train())
    train <- data[1:1200,]
    
    train <- data
    train <- clean$clean(train)
    test_data <- data[1201:1460,]
    test <- test_data
    test <- clean$clean(test)
    test_data <- test_data[, SalePriceOriginal := SalePrice]
    
    result <- prediction$get_result(train, test, test_data)
    rmse_result <- rmse(result$SalePrice, result$SalePriceOriginal)
    rmse_values[i] <- rmse_result
  }
  
  mean(rmse_values)
}



