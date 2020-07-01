# check missing data
# impute data
# convert types of columns if needed
# use factors
# select features

export("clean")

select_features <- function(data) {
  feature_list <- c("SalePrice", "LotArea", "YearBuilt")
  data[, feature_list, with=FALSE]
}

show_missing_data <- function(data) {
  sapply(data,function(x) sum(is.na(x)))
}


clean <- function(data) {
  data <- select_features(data)
  data
}