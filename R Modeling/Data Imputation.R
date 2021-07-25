## Read in variable
data <- read.csv('breast-cancer-wisconsin.data.txt', header = FALSE)
head(data)
data[data == "?"] = NA
data_num <- data
for (i in colnames(data_num)){
    as.numeric(data_num$i)
}
data_num$V7 <- as.numeric(data_num$V7)
data_num[618, ]
data_num[data_num == 11] = NA

## Filter out complete rows
complete_cases <- data[complete.cases(data) == TRUE,]
missing_cases <- data[complete.cases(data) == FALSE, ]

## Impute w/ mean
mean_imputed <- data_num
mean_imputed$V7 <- as.numeric(mean_imputed$V7)
mean_imputed$V7[is.na(mean_imputed$V7)] <- mean(mean_imputed$V7, na.rm = TRUE)
length(complete.cases(mean_imputed))
mean_imputed

## Impute w/ mode
getmode <- function(x) {
    uniquex <- unique(x)
    uniquex[which.max(tabulate(match(x, uniquex)))]
}

mode_imputed <- data
mode_imputed$V7[is.na(mode_imputed$V7)] <- getmode(complete_cases$V7)
length(complete.cases(mode_imputed))
mode_imputed

## Impute w/ regression
install.packages("mice")
library(mice)
preds <- data[,2:10]
preds_num <- data_num[,2:10]

# Use "pmm" model <- treat variables as numeric
reg_impute <- mice(data = preds_num, m = 3, method = "pmm", maxit = 50, seed = 100)
imputed_vals <- reg_impute$imp$V7
reg_imputed <- complete(reg_impute, 1)
reg_imputed

# Use "polyreg" model <- treat variables as factors
reg_impute2 <- mice(data = preds, m = 3, method = "polyreg", maxit = 50, seed = 100)
imputed_vals2 <- reg_impute2$imp$V7
reg_imputed2 <- complete(reg_impute2, 1)
reg_imputed2

## Impute w/ regression w/ perturbation
reg_impute3 <- mice(data = preds_num, m = 3, method = "norm.nob", maxit = 50, seed = 100)
imputed_vals3 <- reg_impute3$imp$V7
reg_imputed3 <- complete(reg_impute3, 1)
reg_imputed3