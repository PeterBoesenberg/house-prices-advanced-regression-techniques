import(data.table)

export("save")

save <- function(data) {
  features <- c("Id", "SalePrice")
  data <- data[, features, with=FALSE]
  
  fwrite(data, "result.csv")
}