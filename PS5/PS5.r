# Problem Set 5
# Christian Hilscher
# 14.07.2021

library(MASS)
library(collections)
library(tree)
library(randomForest)

### Functions

train_test_data <- function(data, n){
  
  # Indicators for test data
  # n is the number of TEST observations
  test_ind <- sample.int(nrow(data), n)
  train_data <- data[-test_ind, ]
  test_data <- data[test_ind, ]
  
  out_dict <- dict()
  out_dict$set("train", train_data)
  out_dict$set("test", test_data)
  
  return(out_dict)
}

mse <- function(pred, y){
  mse <- mean((pred-y)^2)
  return(mse)
}

### Problem 1
df <- Boston

## a.)
n_test <- dim(df)[1]%/%2
data_dict <- train_test_data(df, n_test)

## b.)
t <- tree(medv~., data=data_dict$get("train"))
summary(t)
plot(t)
text(t, pretty=0)

## c.)
cv_t <- cv.tree(t)
plot(cv_t$size, sqrt(cv_t$dev / nrow(data_dict$get("train"))), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")

# Setting best size as optimal tuning parameter
cv_t$dev/nrow(data_dict$get("train"))
opt_size <- cv_t$size[which.min(cv_t$dev)]

pruned_t <- prune.tree(t, best = opt_size)

# Plotting
par(mfrow=c(2, 1))
plot(t)
plot(pruned_t)

## d.)

# Predicting for normal and pruned tree
pred_t <- predict(t, newdata = data_dict$get("test"))
pred_pruned_t <- predict(pruned_t, newdata = data_dict$get("test"))

# Getting MSE for both 
mse(pred_t, data_dict$get("test")$medv)
mse(pred_pruned_t, data_dict$get("test")$medv)

## e.)

# Manual bagging
n_bag <- 500
tree_list <- vector("list", length=n_bag)
pred_mat <- matrix(NA, nrow(df)-n_test, n_bag)

for (b in 1:n_bag){
  d_dict <- train_test_data(df, n_test)
  t <- tree(medv~., data = d_dict$get("train"))
  
  tree_list[[b]] <- t
  pred_mat[, b] <- predict(t, newdata = d_dict$get("test"))
}

# Plot first 4 trees
par(mfrow=c(2,2))

for (i in 1:4){
  plot(tree_list[[i]])
  text(tree_list[[i]], pretty =0)
}

first.split.var <- sapply(1:n_bag, function(i) tree_list[[i]]$frame[1, 1])
class(first.split.var)
table(first.split.var)

# Test sample 
pred_bagging <- rowMeans(pred_mat)
mse(pred_bagging, data_dict$get("test")$medv) # MSE roughly half as large as before

## f.)

rf <- randomForest(medv~., data=data_dict$get("train"), mtry=13)
pred_rf <- predict(rf, newdata=data_dict$get("test"))
mse(pred_rf, data_dict$get("test")$medv) # Even smaller MSE than before
