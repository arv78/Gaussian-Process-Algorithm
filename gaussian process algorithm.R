require(MASS)
require(plyr)
require(reshape2)
require(ggplot2)

set.seed(12345)
# between -10,10 so then we have mean 0
train_x <- c(-9,-5,3,6,8)
train_y <- c(-8,-7,-2,1,9)
Data <- data.frame(x=train_x, y=train_y) 
test_x_1 <- seq(-10,10,len=10)
test_x_2 <- seq(-10,10,len=50)
test_x_3 <- seq(-10,10,len=100)

# this function builds the covariance for our normal distribution to calculate the probability
covariance <- function(vec1,vec2){
  
  sigma <- matrix(rep(0,length(vec1)*length(vec2)), nrow = length(vec1))
  for (i in seq(nrow(sigma))){
    for (j in seq(ncol(sigma))){
      # RBF kernel
      sigma[i,j] <- exp(-0.5*(abs(vec1[i]-vec2[j]))^2)
    }
  }
  return(sigma)
}

k <- covariance(train_x,train_x)
#k*
k_star_1 <- covariance(train_x,test_x_1)
k_star_T_1 <- covariance(test_x_1,train_x)
k_star_2 <- covariance(train_x,test_x_2)
k_star_T_2 <- covariance(test_x_2,train_x)
k_star_3 <- covariance(train_x,test_x_3)
k_star_T_3 <- covariance(test_x_3,train_x)
#k**
k_star_star_1 <- covariance(test_x_1,test_x_1)
k_star_star_2 <- covariance(test_x_2,test_x_2)
k_star_star_3 <- covariance(test_x_3,test_x_3)

# implementing the formula
# N(mean = T(k*) x inverse(k) x y, covariance = k** - (T(k*) x inverse(k) x k*))
# note: solve(k) = inverse(k)
mean1 <- k_star_T_1 %*% solve(k) %*% train_y
cov1 <- k_star_star_1 - k_star_T_1 %*% solve(k) %*% k_star_1
mean2 <- k_star_T_2 %*% solve(k) %*% train_y
cov2 <- k_star_star_2 - k_star_T_2 %*% solve(k) %*% k_star_2
mean3 <- k_star_T_3 %*% solve(k) %*% train_y
cov3 <- k_star_star_3 - k_star_T_3 %*% solve(k) %*% k_star_3

num_samples1 <- 10
num_samples2 <- 50
num_samples3 <- 100

# getting n number of samples based on the demanded mean and covariance so at the end
# we will have some what close mean and covariance for a certain variable(sample) each time
values1 <- matrix(rep(0,length(test_x_1)* num_samples1), ncol=num_samples1)
for (i in 1:num_samples1) {
  values1[,i] <- mvrnorm(1, mean1, cov1)
}
values1 <- cbind(x=test_x_1,as.data.frame(values1))
values1 <- melt(values1,id="x")

values2 <- matrix(rep(0,length(test_x_2)* num_samples2), ncol=num_samples2)
for (i in 1:num_samples2) {
  values2[,i] <- mvrnorm(1, mean2, cov2)
}
values2 <- cbind(x=test_x_2,as.data.frame(values2))
values2 <- melt(values2,id="x")

values3 <- matrix(rep(0,length(test_x_3)* num_samples3), ncol=num_samples3)
for (i in 1:num_samples3) {
  values3[,i] <- mvrnorm(1, mean3, cov3)
}
values3 <- cbind(x=test_x_3,as.data.frame(values3))
values3 <- melt(values3,id="x")

# Plot the result including the mean and covariance
# aes basically map the x and y so we say what is the x and y in data="something"
fig1 <- ggplot() +
  geom_line(data=values1, aes(x=x,y=value, group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=test_x_1,y=mean1),colour="red", size=1) + 
  geom_point(data=Data,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-10,10), name="output, f(x)") +
  xlab("input, x") +
  ggtitle("10 parameters")
fig1

fig2 <- ggplot() +
  geom_line(data=values2, aes(x=x,y=value, group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=test_x_2,y=mean2),colour="red", size=1) + 
  geom_point(data=Data,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-10,10), name="output, f(x)") +
  xlab("input, x") +
  ggtitle("50 parameters")
fig2

fig3 <- ggplot() +
  geom_line(data=values3, aes(x=x,y=value, group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=test_x_3,y=mean3),colour="red", size=1) + 
  geom_point(data=Data,aes(x=x,y=y)) +
  theme_bw() +
  scale_y_continuous(lim=c(-10,10), name="output, f(x)") +
  xlab("input, x") +
  ggtitle("100 parameters")
fig3