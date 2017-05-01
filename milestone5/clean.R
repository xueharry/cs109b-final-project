## PREPARE DATA ##

# load data
test <- read.csv('full_data_test.csv')

# make zero budget and zero revenue NA
test$budget[test$budget == 0] <- NA
test$revenue[test$revenue == 0] <- NA

# impute all columns by column median
for(i in 1:ncol(test)){
    test[is.na(test[,i]), i] <- median(test[,i], na.rm = TRUE)
}

# drop X column
test <- test[, -1]

# save
write.csv(test, 'clean_test.csv')