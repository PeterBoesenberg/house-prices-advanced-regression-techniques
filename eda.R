library(corrplot)
library(modules)
load <- modules::use("load.R")

train <- load$load_train()

res <- cor(train, method = c("pearson", "kendall", "spearman"))
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)