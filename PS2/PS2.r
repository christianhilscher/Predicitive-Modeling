# Problem Set 2
# Christian Hilscher
# 07.07.2021

library(ggplot2)
library(nycflights13)

## Problem 1

predict <- function(x, y, k, x0_single){
  # Get the rank of the absolute deviation compared to training data
  dist <- abs(x-x0_single)
  rank_vec <- rank(dist, ties.method = "random")
  
  # Indices of outcome variable where rank is sufficiently close
  neighbor_indices <- rank_vec <= k
  
  # Return the mean of those neighboring Y-values
  return(mean(y[neighbor_indices]))
}


kNN_prediction <- function(x, y, k, x0){
  
  # Making sure that input lengths are the same
  stopifnot(length(x)==length(y))
  
  # Allocating space
  result_arr <- array(numeric(), length(x0))
  
  for(i in seq_along(x0)){
    result_arr[i] = predict(x, y, k, x0[i])
  }
  
  return(result_arr)
}

## Problem 2

#### a.)
df <- nycflights13::flights

# Filtering given arrival and departure delay
df_clean <- na.omit(df[c("dep_delay", "arr_delay")])

### b.)
x0 <- seq(-10, 50, length.out=100)

k_list <- c(1, 20, 200)
result_mat <- array(numeric(), c(length(x0), length(k_list)))

for(k in seq_along(k_list)){
  result_mat[,k] = kNN_prediction(df_clean$dep_delay, df_clean$arr_delay, k_list[k], x0)
}

pred <- data.frame(result_mat)
pred$source <- x0

### c.)
ggplot(data=pred, aes(x=source)) + 
  geom_line(aes(y=X1), color="darkred") + 
  geom_line(aes(y=X2), color="steelblue") + 
  geom_line(aes(y=X3))

## Problem 3

mse = function(res){
  n = length(res)
  mse = mean(res**2)
  return(mse)
}
aic = function(res, k){
  n = length(res)
  aic = n*log(sum(res**2)/n)+2*k
  return(aic)
}
bic = function(res, k){
  n = length(res)
  bic = n*log(sum(res**2)/n)+k*log(n)
  return(bic)
}
loo_mse = function(res, mod){
  res_loo = res/(1-hatvalues(mod))
  loo_mse = mean(res_loo**2)
  return(loo_mse)
}

# Function for getting all subsets of a given dataframe
get_combinations <- function(df){
  # Making space
  l <- c()
  
  # For each column get column names
  for(m in seq_along(colnames(df))){
    
    # Need double loop to get every combination
    names_to_add = combn(colnames(df), m=m)
    for(i in 1:dim(names_to_add)[2]){
      l <- append(l, list(names_to_add[,i]))
    }
  }
  return(l)
}

# Function for performing subset selection
subset_selection <- function(df, dep_var){
  
  # First get the combinations
  subsets <- get_combinations(df)
  # Allocating space
  eval_matrix <- array(numeric(), c(length(subsets), 4))
  
  # Looping through the subsets
  for(i in seq_along(subsets)){
    
    # Unpacking the list of combinations
    regressors <- df[unlist(subsets[i])]
    
    # Estimation
    k = dim(regressors)[2]
    reg_df <- cbind(dep_var, regressors)
    model <- lm(dep_var ~ ., reg_df)
    res <- summary(model)$residuals
    
    # Calculate evaluation scores
    eval_matrix[i, 1] = mse(res)
    eval_matrix[i, 2] = aic(res, k)
    eval_matrix[i, 3] = bic(res, k)
    eval_matrix[i, 4] = loo_mse(res, model)
  }
  
  # Print which one was best
  evals = c('mse', 'aic', 'bic', 'loo_mse')
  for (i in 1:dim(eval_matrix)[2]){
    where_min = which.min(eval_matrix[i, ])
    best_mod = paste(unlist(subsets[where_min]), collapse = ' + ')
    phrase = paste('Best model according to', evals[i], 'is: ', sep = ' ')
    print(paste(phrase, best_mod, 'with', eval_matrix[i, where_min], sep = ' '))
  }
}

df_clean <- na.omit(df[c("dep_delay", "arr_delay", "distance", "day")])
df_x <- df_clean[c("dep_delay", "distance", "day")]
dep_var <- df_clean$arr_delay

subset_selection(df_x, dep_var)

