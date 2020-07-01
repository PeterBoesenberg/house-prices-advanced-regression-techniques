import(data.table)

export("load_train", "load_test")

load_train <- function() {
  fread("data/train.csv")
}

load_test <- function() {
  fread("data/test.csv")
}