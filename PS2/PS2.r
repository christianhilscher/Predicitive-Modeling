# Problem Set 2
# Christian Hilscher
# 07.07.2021

### Problem 1

predict <- function(x, y, k, x0_single){
  
  # Get the rank of the absolute deviation compared to training data
  rank_vec <- rank(abs(x-x0_single), ties.method = "random")
  
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
  
  for(i in 1:length(x0)){
    result_arr[i] = predict(x, y, k, x0[i])
  }
  
  return(result_arr)
}
