# Problem Set 4
# Christian Hilscher
# 08.07.2021

library(AmesHousing)
library(dplyr)
library(collections)
library(glmnet)

## Problem 1
df <- make_ames()
pred_mat <- df %>%
  select(-c(Sale_Condition, Sale_Type))

# Dropping constant predictors
pred_mat <- pred_mat[sapply(pred_mat, function(x) length(unique(na.omit(x)))) > 1]

## Problem 2

prep_data <- function(data){
  
  # Taking random 1000 observations
  test_ind <- sample.int(nrow(pred_mat), 1000)
  
  X <- model.matrix(Sale_Price~., data=data)[, -1]
  y <- data$Sale_Price
  
  # Delete duplicates
  dups <- which(apply(X, 2, sd) == 0)
  X <- X[,-dups]
  
  # Split into train and test data
  X_train <- X[-test_ind, ]
  X_test <- X[test_ind, ]
  y_train <- y[-test_ind]
  y_test <- y[test_ind]
  

  
  # Making object for output
  out_dict <- dict()
  
  # Saving stuff in dictionary
  out_dict$set("xtrain", X_train)
  out_dict$set("xtest", X_test)
  out_dict$set("ytrain", y_train)
  out_dict$set("ytest", y_test)
  
  
  
  return(out_dict)
}


### OLS Model
data <- prep_data(pred_mat)
ols_model <- lm(data$get("ytrain")~data$get("xtrain"))

### CV Ridge-Regression
cv_ridge <- cv.glmnet(data$get("xtrain"), data$get("ytrain"), alpha = 1)
# plot(cv_ridge)
best_ridge <- cv_ridge$glmnet.fit # Save best fit


### CV Laso-Regression
cv_lasso <- cv.glmnet(data$get("xtrain"), data$get("ytrain"), alpha = 0)
# plot(cv_lasso)
best_lasso <- cv_lasso$glmnet.fit # Save best fit


pred_ols <- predict.lm(ols_model, newdata=data.frame(data$get("xtest")))
pred_ridge <- predict.glmnet(best_ridge, s = cv_ridge$lambda.min, newx=data$get("xtest"))
pred_lasso <- predict.glmnet(best_lasso, s = cv_lasso$lambda.min, newx=data$get("xtest"))



mse <- function(pred, y){
  mse <- mean((pred-y)^2)
  return(mse)
}
