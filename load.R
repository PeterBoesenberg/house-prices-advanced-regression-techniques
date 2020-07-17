import(data.table)

export("load_train", "load_test")


randomize_order <- function(data) {
  set.seed(1)
  data <- data[sample(1:nrow(data)), ]
  data
}


load_train <- function() {
  data <- as.data.table(fread("data/train.csv"))
  randomize_order(data)
}

load_test <- function() {
  data <- as.data.table(fread("data/test.csv"))
  randomize_order(data)
}