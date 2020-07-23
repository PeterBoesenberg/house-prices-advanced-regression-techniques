import(stats)
import(data.table)
import(caret)
import(caretEnsemble)
export("get_result", "get_lm_result", "get_svm_result", "get_svm_all_methods_results")

svm_methods <- c(
                 "svmLinear",
                 "svmPoly",
                 "svmRadial",
                 "svmRadialCost",
                 "svmRadialSigma"
                 )

get_lm_result <- function(train, test, test_data) {
  model <- lm(SalePrice ~., data=train)
  prediction <- predict(model, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data[is.na(SalePrice), SalePrice:=mean(train$SalePrice)]
  test_data
}

get_svm_result <- function(train, test, test_data, method) {
  model <- train(SalePrice ~ ., data = train, method=method)
  prediction <- predict(model, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data[is.na(SalePrice), SalePrice:=mean(train$SalePrice)]
  test_data
}

get_svm_all_methods_results <- function(train, test, test_data) {
  test_data_results <- list()
  for(i in 1:length(svm_methods)) {
    method <- svm_methods[i]
    test_data_results[[method]] <- get_svm_result(train, test, test_data, method)
  }
  test_data_results
}

get_stacked_result <- function(train, test, test_data) {
  trainingControl <- trainControl(method='cv', 
                                  number=10, 
                                  savePredictions = "final", 
                                  classProbs=TRUE, 
                                  index=createResample(train$SalePrice))
  # algorithms_to_use <- c( "ridge", "lasso", "glm", "enet","svmLinear", "xgbDART")
  algorithms_to_use <- c( "ridge",  "xgbLinear")
  stacked_models <- caretList(SalePrice ~ ., data=train, trControl=trainingControl, methodList=algorithms_to_use)
  stacking_results <- resamples(stacked_models)
  
  stackControl <- trainControl(method="repeatedcv", number=5, repeats=3, savePredictions=TRUE, classProbs=TRUE)
  glm_stack <- caretStack(stacked_models, method="gbm",  trControl=stackControl)
  prediction <- predict(glm_stack, test)
  test_data <- test_data[, SalePrice:=prediction]
  test_data[is.na(SalePrice), SalePrice:=mean(train$SalePrice)]
  test_data
}

get_result <- function(train, test, test_data) {
  get_stacked_result(train, test, test_data)
}

