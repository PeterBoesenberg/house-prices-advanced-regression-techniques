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

result <- prediction$get_result(train, test, test_data)

save$save(result)

