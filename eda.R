library(corrplot)
library(modules)
library(ggpubr)
load <- modules::use("load.R")
clean <- modules::use("clean.R")

train <- load$load_train()
train <- clean$clean(train)

check_correlations <- function(data) {
  res <- cor(data, method = c("pearson", "kendall", "spearman"))
  corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
}

check_normal_distribution <- function(data) {
  dist_test <- sapply(data, shapiro.test)
  dist_test <- as.data.table(t(as.data.table(dist_test, keep.rownames = TRUE)[2]), keep.rownames = TRUE)[-1]
  dist_test[, is_normal:=V1>0.05]
  dist_test
}


check_correlations(train)
distributions <- check_normal_distribution(train)
print(distributions)