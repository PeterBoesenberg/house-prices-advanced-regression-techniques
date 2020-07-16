import(stats)
import(data.table)
import(e1071)
export("get_result", "get_lm_result", "get_svm_result")

get_lm_result <- function(train, test, test_data) {
  model <- lm(SalePrice ~., data=train)
  prediction <- predict(model, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data[is.na(SalePrice), SalePrice:=mean(train$SalePrice)]
  test_data
}

get_svm_result <- function(train, test, test_data) {
  model <- svm(SalePrice ~ ., data = train)
  prediction <- predict(model, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data[is.na(SalePrice), SalePrice:=mean(train$SalePrice)]
  test_data
}

get_result <- function(train, test, test_data) {
  get_svm_result(train, test, test_data)
}
