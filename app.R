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

run <- function() {
  if(is_kaggle_mode) {
    # prepare data variables for submission
    train <- load$load_train()
    train <- clean$clean(train, TRUE)
    test_data <- load$load_test()
    test <- test_data
    test <- test[, SalePrice:=0]
    test <- clean$clean(test, FALSE)
    
    result <- prediction$get_result(train, test, test_data)
    save$save(result)
  } else {
    # prepare data variables for local test, use test-train-split
    data <- as.data.table(load$load_train())
    train <- data[1:1200,]
    
    train <- data
    train <- clean$clean(train, TRUE)
    test_data <- data[1201:1460,]
    test <- test_data
    test <- clean$clean(test, FALSE)
    test_data <- test_data[, SalePriceOriginal := SalePrice]
    
    result <- prediction$get_result(train, test, test_data)
    rmse_result <- rmse(result$SalePrice, result$SalePriceOriginal)
    
    print("FINAL RESULT")
    print(rmse_result)
    
  }
}


system.time({ run() })
