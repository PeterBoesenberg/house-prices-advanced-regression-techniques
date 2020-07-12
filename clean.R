import(data.table)
# convert types of columns if needed
# use factors
# select features
# MoSold + YrSold as one DateColumn?
export("clean")

numeric_features <-c("LotFrontage",
                     "YearBuilt", 
                     "YearRemodAdd",
                     "MasVnrArea",
                     "BsmtFinSF1",
                     "BsmtFinSF2",
                     "BsmtUnfSF",    
                     "TotalBsmtSF",   "1stFlrSF" ,     "2ndFlrSF"    ,  "LowQualFinSF" , "GrLivArea"   ,  "BsmtFullBath", 
                     "BsmtHalfBath",  "FullBath",      "HalfBath"     , "BedroomAbvGr",  "KitchenAbvGr" , "TotRmsAbvGrd" ,
                     "Fireplaces",    "GarageYrBlt",   "GarageCars"  ,  "GarageArea"   , "WoodDeckSF"   , "OpenPorchSF",  
                     "EnclosedPorch", "3SsnPorch",     "ScreenPorch"  , "PoolArea",
                     "MiscVal", 
                     "MoSold", 
                     "YrSold")

select_features <- function(data) {
  feature_list <- c("SalePrice", numeric_features)
  data[, feature_list, with=FALSE]
}

show_missing_data <- function(data) {
  sapply(data,function(x) sum(is.na(x)))
}

# replace missing values in numeric columns with mean of this col
impute_missing_values <- function(data) {
  numeric_cols <- names(which(sapply(data, is.numeric)))
  for (j in numeric_cols) {
    set(data, which(is.na(data[[j]])), j , mean(data[, get(j)], na.rm=TRUE))
  }
  data
}

clean <- function(data) {
  data <- select_features(data)
  data <- impute_missing_values(data)
  data
}
