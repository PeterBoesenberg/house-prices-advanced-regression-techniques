library(data.table)
library(h2o)

  h2o.init()
  
  train <- h2o.importFile("data/train.csv")
  test <- h2o.importFile("data/test.csv")
  
  # Identify predictors and response
  y <- "SalePrice"
  x <- setdiff(names(train), y)
  
  
  aml <- h2o.automl(x = x, y = y,
                    training_frame = train,
                    max_models = 20,
                    max_runtime_secs = 10,
                    seed = 1)
  
  # View the AutoML Leaderboard
  lb <- aml@leaderboard
  pred <- h2o.predict(aml@leader, test)
  test <- as.data.table(test)
  pred <- as.vector(pred)

  test<-test[, SalePrice:=pred][, list(Id, SalePrice)]
  fwrite(test, "result.csv")

