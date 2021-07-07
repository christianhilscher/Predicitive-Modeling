# Problem Set 1
# Christian Hilscher
# 07.07.2021



### Problem 1
a <- seq(1, 20, by=2)
b <- rep(5, 20)
c <- c("We are the Champions", "Bohemian Rhapsody", "Antoher One Bites the Dust")
d <- rnorm(100)
e <- matrix(seq(1, 25), nrow=5, ncol=5, byrow=TRUE)
f <- matrix(runif(20*20), nrow=20, ncol=20)
g <- matrix()
f <- matrix(rbinom(n=20*2, size=1, prob=0.5), nrow = 20, ncol=2)
h <- data.frame(a=seq(1,20), b=LETTERS[1:20])

# Problem 2
df <- nycflights13::flights

a <- which.max(df$air_time)
b <- mean(df$distance)

df_short <- df[1:5000,]
plot(df_short$air_time, df_short$distance, main="scatter")
hist(df_short$air_time)

# Problem 3
p3_1 <- function(a, b){
  return (a*b)
}

p3_2 <- function(x){
  if (x %% 2 == 0){
    return ("Number is even")
  }else {
    return ("Number is odd")
  }
}