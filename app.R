library(plotly)
library(data.table)
library(modules)

load <- modules::use("load.R")
clean <- modules::use("clean.R")
save <- modules::use("save.R")
prediction <- modules::use("predict.R")

train <- load$load_train()
train <- clean$clean(train)
test_data <- load$load_test()
test <- test_data
test <- test[, SalePrice:=0]
test <- clean$clean(test)

# data <- as.data.table(load$load_train())
# 
# 
# train <- data[1:1200,]
# train <- clean$clean(train)
# test_data <- data[1201:1460,]
# test <- test_data
# test <- clean$clean(test)
# 
# test_data <- test_data[, SalePriceOriginal := SalePrice]

result <- prediction$get_result(train, test, test_data)


deviation <- sqrt(mean(abs(result$SalePrice - result$SalePriceOriginal)^2))
deviation <- mean(abs(result$SalePrice - result$SalePriceOriginal))
deviation

save$save(result)

