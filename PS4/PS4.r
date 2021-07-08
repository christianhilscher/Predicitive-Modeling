# Problem Set 4
# Christian Hilscher
# 08.07.2021

library(AmesHousing)
library(dplyr)

## Problem 1
df <- make_ames()
pred_mat <- df %>%
  select(-c(Sale_Condition, Sale_Type))

# Dropping constant predictors
pred_mat <- pred_mat[sapply(pred_mat, function(x) length(unique(na.omit(x)))) > 1]

## Problem 2

prep_data <- function(data){
  test_ind <- sample.int(nrow(pred_mat), 1000)
  data_test <- pred_mat[test_ind, ]
  data_train <- pred_mat[-test_ind, ]
  
  
  X_train <- model.matrix(Sale_Price~., data=data_train)[, -1]
  y_train <- data_train$Sale_Price
  
  X_test <- model.matrix(Sale_Price~., data=data_test)[, -1]
  y_test <- data_test$Sale_Price
  
  # Duplicate columns the same for train and test 
  dups <- which(apply(data, 2, sd) == 0)
  X_train <- X_train[, -dups]
  X_test <- X_test[, -dups]
  
  return(list(X_train, X_test, y_train, y_test))
}

data <- prep_data(pred_mat)




# OLS estimation
model <- lm(y~X)
summary(model)
predict(model, )
