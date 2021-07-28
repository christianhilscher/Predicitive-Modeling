library(glmnet)
library(randomForest)
library(dplyr)
library(reshape2)
library(plotROC)
library(ggplot2)


setwd("~/UniMA/4th Semester/Predicitive-Modeling/Exam")
heart <- read.csv("heart.csv")
set.seed(2021)


########## Problem 1.1

prep_data <- function(dataf, make_matrix=FALSE){
  # Equal size
  test_ind <- sample.int(nrow(dataf), nrow(dataf)%/%2)
  
  if (make_matrix){
    X <- model.matrix(y~., data=dataf)[, -1]
    y <- dataf$y
    
    out_list <- list("x_train" = X[-test_ind,], 
                     "y_train" = y[-test_ind],
                     "x_test" = X[test_ind,],
                     "y_test" = y[test_ind])
  }
  else{
    out_list <- list("train" = dataf[-test_ind,],
                     "test" = dataf[test_ind,])
  }
  
  return(out_list)
}

get_important_features <- function(rf, data_list, n){
  
  feat_imp_df <- importance(rf) %>% 
    data.frame() %>% 
    mutate(feature = row.names(.)) 
  
  feats <- feat_imp_df %>% arrange(desc(X.IncMSE)) %>% select(feature) %>% slice(1:n)
  
  x_train <- data.frame(data_list$x_train)[feats$feature]
  x_test <- data.frame(data_list$x_test)[feats$feature]
  
  out_list <- list("x_train" = as.matrix(x_train),
                   "x_test" = as.matrix(x_test))
  return(out_list)
}


l <- prep_data(heart, TRUE)
predictions <- data.frame(y = l$y_test)

# Normal Logit

logit.fit <- glm(l$y_train~l$x_train, family="binomial")
predictions$logit <- predict(logit.fit, newdata = data.frame(l$x_test), type="response")

# Lasso Logit 
cv.fit <- cv.glmnet(l$x_train, l$y_train, alpha=1, family="binomial")
predictions$lasso.min <- predict(cv.fit, s = cv.fit$lambda.min, newx = l$x_test, type="response")
predictions$lasso.1se <- predict(cv.fit, s = cv.fit$lambda.1se, newx = l$x_test, type="response")

# RF
rf <- randomForest(l$x_train, l$y_train, importance = TRUE)
predictions$rf <- predict(rf, newx = l$x_test, type="response")

# Nonparametric with most important variables from forest
nonp.data <- get_important_features(rf, l, 3)
nonp.model <- loess(l$y_train~nonp.data$x_train)
predictions$nonp <- predict(nonp.model, newdata = data.frame(l$x_test), type="response")


########## Evaluation of predictions
pred_long <- melt(predictions, id.vars = "y", variable.name = "method", value.name = "value")

# Get Log-Scores and MSE
pred_long %>% group_by(method) %>% summarise(MSE = mean((y-value)^2, na.rm=TRUE),
                                             LogScore = mean(-y*log(value) - (1-y)*log(1-value), na.rm=TRUE))

# Plot ROC and calculate AUC
p.ROC <- ggplot(data = pred_long) + 
  geom_roc(aes(d=y, m=value, color=method)) +
  xlab("False alarm rate") + 
  ylab("Hit rate") + 
  theme(legend.position = "bottom")
p.ROC

p.ROC + facet_wrap(~method, nrow=2)

calc_auc(p.ROC)