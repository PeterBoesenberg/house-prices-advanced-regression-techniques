import(data.table)
import(stats)
import(caret)
export("clean")

numeric_features <- c("LotFrontage",
                     "YearBuilt", 
                     "YearRemodAdd",
                     "MasVnrArea",
                     # "BsmtFinSF1",
                     "BsmtFinSF2",
                     "BsmtUnfSF",    
                     "TotalBsmtSF",   "1stFlrSF" ,     "2ndFlrSF"    ,  
                     # "LowQualFinSF" , 
                     "GrLivArea"   ,  "BsmtFullBath", 
                     "BsmtHalfBath",  "FullBath",      "HalfBath"     , "BedroomAbvGr",  
                     "KitchenAbvGr" , "TotRmsAbvGrd" ,
                     "Fireplaces",    "GarageYrBlt",   "GarageCars"  ,  "GarageArea"   , "WoodDeckSF"   , "OpenPorchSF",  
                     "EnclosedPorch", "3SsnPorch",     "ScreenPorch"  , "PoolArea",
                     "MiscVal", 
                     "MoSold", 
                     "YrSold")

factor_features <- c(
  # "MSSubClass" 
  # ,
  # ,
  "RoofMatl",
  # "Street",
  # "Condition1",
  # "Condition2",
  # "LotShape",
  # "LandContour",
  # "LotConfig",
  # "LandSlope",
  # "Neighborhood",
  "MSZoning"
  # "BldgType", "HouseStyle", "OverallQual", "OverallCond","RoofStyle","Exterior2nd","Exterior1st","Foundation"
                     )

factors_MSSubClass <- c("20","30","40","45","50","60","70","75","80","85","90","120","150","160","180","190")
factors_MSZoning <- c("A","C","FV","I","RH","RP","RL","RM")
factors_Street <- c("Grvl","Paved")
factors_RoofMatl <- c("ClyTile","CompShg","Membran","Metal","Roll","Tar&Grv","WdShake","WdShngl")
factors_Condition1 <- c("Artery","Feedr","Norm","RRNn","RRAn","PosN","PosA","RRNe","RRAe")
factors_LotShape <- c("Reg","IR1","IR2","IR3","IR4")
factors_LandContour <- c("Lvl","Bnk","HLS","Low")
factors_LotConfig <- c("Inside","Corner","CulDSac","FR2","FR3")
factors_LandSlope <- c("Gtl","Mod", "Sev")
factors_OverallCond <- as.character(c(1:10))
factors_Exterior <- c("AsbShng","AsphShn","BrkComm","BrkFace","CBlock","CemntBd","HdBoard","ImStucc","MetalSd","Other","Plywood","PreCast","Stone","Stucco","VinylSd","Wd Sdng","WdShing")
factory_HouseStyle <- c("1.5Fin", "1.5Unf","1Story", "2.5Fin", "2.5Unf", "2Story", "SFoyer")


select_features <- function(data) {
  feature_list <- c("SalePrice", numeric_features)
  feature_list <- c(feature_list, factor_features)
  print("FEATURES")
  print(feature_list)
  # data[, MSSubClass:= factor(MSSubClass, factors_MSSubClass)]
  # data[, MSZoning:= factor(MSZoning, factors_MSZoning)]
  # data[, Street:= factor(Street, factors_Street)]
  data[, RoofMatl:= factor(RoofMatl, factors_RoofMatl)]
  # data[, Condition1:= factor(Condition1, factors_Condition1)]
  # data[, Condition2:= factor(Condition2, factors_Condition1)]
  # data[, LotShape:= factor(LotShape, factors_LotShape)]
  # data[, LandContour:= factor(LandContour, factors_LandContour)]
  # data[, LotConfig:= factor(LotConfig, factors_LotConfig)]
  # data[, LandSlope:= factor(LandSlope, factors_LandSlope)]
  # data[, Neighborhood:= as.factor(Neighborhood)]
  # data[, BldgType:= as.factor(BldgType)]
  # data[, HouseStyle:= factor(HouseStyle, factory_HouseStyle)]
  # data[, OverallQual:= as.factor(OverallQual)]
  # data[, OverallCond:= factor(OverallCond, factors_OverallCond)]
  # data[, RoofStyle:= as.factor(RoofStyle)]
  # data[, Exterior1st:= factor(Exterior1st, factors_Exterior)]
  # data[, Exterior2nd:= factor(Exterior2nd, factors_Exterior)]
  # data[, Foundation:= as.factor(Foundation)]

  low_variance_features <- as.data.table(nearZeroVar(data, saveMetrics= TRUE), keep.rownames = T)[nzv==TRUE, rn]
  feature_list <- feature_list[!(feature_list %in% low_variance_features)]
  
  data <- data[, feature_list, with=FALSE]
  data
}

show_missing_data <- function(data) {
  sapply(data,function(x) sum(is.na(x)))
}

# replace missing values in numeric columns with mean of this col
# in non-numeric columns with most common value
impute_missing_values <- function(data) {
  numeric_cols <- names(which(sapply(data, is.numeric)))
  for (j in numeric_cols) {
    set(data, which(is.na(data[[j]])), j , mean(data[, get(j)], na.rm=TRUE))
  }
  non_numeric_cols <- names(which(!sapply(data, is.numeric)))
  for (j in non_numeric_cols) {
    set(data, which(is.na(data[[j]])), j , names(sort(table(data[,get(j)]),decreasing = TRUE)[1]))
  }
  data
}

clean <- function(data) {
  data <- impute_missing_values(data)
  data <- select_features(data)
  data <- impute_missing_values(data)
  data
}
