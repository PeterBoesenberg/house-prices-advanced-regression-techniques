import(stats)

export("get_result")

get_result <- function(train, test, test_data) {
  model <- lm(SalePrice ~.,data=train)
  prediction <- predict(model, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data
}