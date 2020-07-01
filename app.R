library(plotly)
library(data.table)
library(modules)

load <- modules::use("load.R")
clean <- modules::use("clean.R")
save <- modules::use("save.R")

train <- load$load_train()
train <- clean$clean(train)

test_data <- load$load_test()
test <- test_data


model <- lm(SalePrice ~.,data=train)

# summary(model)


prediction <- predict(model, newdata=test)
str(test)
dim(test_data)
test_data <- test_data[, SalePrice:=prediction]

res <-  test_data$SalePrice - prediction
rmse <- sqrt(mean(res ^ 2))
print(rmse)



save$save(test_data)

