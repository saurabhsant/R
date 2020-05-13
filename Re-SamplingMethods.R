#Saurabh Sant

# Set the directory to the data folder
setwd("/home/tahir/Downloads/")

# Read in the data. This is a 200x5 data frame
data <- read.table("lifetime.txt", header= FALSE)

# Re-Shape to make it 1000x1 data frame
values <- c(data$V1, data$V2, data$V3, data$V4, data$V5)
data <-  data.frame(values)
# Rename the null column to be X. Now X denotes lifetime. 
names(data)[1] <- "X"

# We have finished data pre-processeing. 
# We now attach it for easier accessing.
attach(data)

# Some libraries used on later
require(graphics)

# Display our dataset
View(data)

# Now, we can just access the variable X which has all of our values. 


#########################################
######        Course: STA312       ######
###### Final Project: Question 3A  ######
###### Sant Saurabh   | 1002434047 ######
#########################################


# Find E(theta), the expected value
theta <- mean(X) # This is the point estimate
print(theta)

# Q: Find the confidence Interval for Theta
# The confidence interval takes the form
# thetahat +- criticalValue * se(thetahat). 
# We don't know the se(thetahat) as it is just one value. 
# Thus, we resample using bootstrample, and find se(theta).

# seed for replicapability
set.seed(2020)

# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size
R <- numeric(B)         # storage for replicates

# Bootstrapping Estimate of standard deviation
for (b in 1:B){
  i <- sample(1:n, size = n, replace=TRUE)
  X.boot <- X[i]
  # Store the mean of each sample
  R[b] <-mean(X.boot)
}
# R is a vector of average lifetime for each sample
print(R)
boostrapped.theta <- mean(R)
bootstrapped.sd <- sd(R)
# Print bootstrapped results
print(c(boostrapped.theta, bootstrapped.sd)) 

# Now we need  to find the last piece of the confidence interval, the critical value. 
# For the critical value, we look at our sampling distribution of the thetas and see if it is normal. 
par(mfrow=c(1,3))
hist(R)
qqnorm(R)
boxplot(R)

# After looking at the plots, we can conclude normality seems to hold. 
# hence, we can apply CLT and the normal distribution for the critical values. 

Lower_tail <- theta - qnorm(0.975)*(bootstrapped.sd/sqrt(length(X)))
Upper_tail <- theta + qnorm(0.975)*(bootstrapped.sd/sqrt(length(X)))

# Thus, our 95% CI for theta is: 
print(c(Lower_tail,Upper_tail))

# The expected value (Point estimate)
print(theta)


#########################################
######        Course: STA312       ######
###### Final Project: Question 3B  ######
###### Sant Saurabh   | 1002434047 ######
#########################################


# The actual median from the sample (Point estimate)
theta <- median(X)
print(theta)

# We are asked to find the bootstrapped median
# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size
R <- numeric(B)         # storage for replicates

# Bootstrapping Estimate of the Median
for (b in 1:B){
  i <- sample(1:n, size = n, replace=TRUE)
  X.boot <- X[i]
  # Store the median of each sample
  R[b] <- median(X.boot)
}
# R is a vector of median lifetime for each sample
# print(R)
boostrapped.theta <- median(R)
# Print bootstrapped results
print(boostrapped.theta)

# We can see that the sampling distribution of the median is not necessiarly normal.
# Thus, we use the percentile CI.
par(mfrow=c(1,3))
hist(R,breaks=floor(sqrt(B)),freq=F)
qqnorm(R)
boxplot(R)

# Thus, to find the percentile confidence interval, we provide the percentile procedure
# as the sampling distribution is not normal. 
alpha=0.05

# The 95% quantile for Median
ninety_five_quantile <- quantile(R, 1-alpha)
print(ninety_five_quantile)
  
# The percentile CI for Median Sampling distribution
lower_q <- quantile(R,alpha/2)
upper_q <- quantile(R, 1 - (alpha/2))
print((c(lower_q, upper_q)))

####################################
############  KURTOSIS  ############
####################################

# The actual kurtosis from the sample (Point estimate)
n <- length(X)
theta = ( sum((X - mean(X))^4 )/n ) / (var(X)^2)
print(theta)

# We are asked to find the bootstrapped kurtosis
# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size
R <- numeric(B)         # storage for replicates

# Bootstrapping Estimate of the Kurtosis
for (b in 1:B){
  i <- sample(1:n, size = n, replace=TRUE)
  X.boot <- X[i]
  # Store the median of each sample
  R[b] <- ( sum((X.boot - mean(X.boot))^4 )/n ) / (var(X.boot)^2)
}
# R is a vector of median lifetime for each sample
# print(R)
boostrapped.theta <- ( sum((R - mean(R))^4 )/n ) / (var(R)^2)
# Print bootstrapped results
boostrapped.theta

# We can see that the sampling distribution of the median is not necessiarly normal.
par(mfrow=c(1,3))
hist(R,breaks=floor(sqrt(B)),freq=F)
qqnorm(R)
boxplot(R)

# Thus, to find the percentile confidence interval, we provide the percentile procedure
# as the sampling distribution is not normal. 
alpha=0.05

# The 95% quantile for Kurtosis Sampling distribution
ninety_five_quantile <- quantile( R, 1-alpha)
print(ninety_five_quantile)

# The 95% percentile CI
lower_q <- quantile(R,alpha/2)
upper_q <- quantile(R, 1 - (alpha/2))
print((c(lower_q, upper_q)))

#################################################
############  95% Quantile Estimate  ############
#################################################

# We are asked to find the bootstrapped 95% Quartile
# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size
R <- numeric(B)         # storage for replicates

# Non-Parametric Bootstrapping Estimate of the 95% Quartile
for (b in 1:B){
  i <- sample(1:n, size = n, replace=TRUE)
  X.boot <- X[i]
  # Store the median of each sample
  R[b] <- quantile(X.boot, 0.95)
}
# R is a vector of 95% quantile estimates lifetime for each sample
# print(R)
boostrapped.theta <- quantile(R,0.95)
# Print bootstrapped results
print(boostrapped.theta)

# We can see that the sampling distribution of the median is not necessiarly normal.
# Thus, we use the percentile CI.
par(mfrow=c(1,3))
hist(R,breaks=floor(sqrt(B)),freq=F, main="Non-Parametric 95% Quantile Estimate")
qqnorm(R)
boxplot(R)

# Thus, to find the percentile confidence interval, we provide the percentile procedure
# as the sampling distribution is not normal. 
alpha=0.05

# The percentile CI for Median Sampling distribution
lower_q <- quantile(R,alpha/2)
upper_q <- quantile(R, 1 - (alpha/2))
print((c(lower_q, upper_q)))


#########################################
######        Course: STA312       ######
###### Final Project: Question 3C  ######
###### Sant Saurabh   | 1002434047 ######
#########################################

# To fund a estimator for exponential(lambda), we use the MLE and get it to be 1/xbar
# Therefore, lambda-hat = 1/x-bar

# The MLE estimator, Lambda-hat
lambda.hat <- 1/mean(X)
print(lambda.hat)

# STEP 1) 
# We look at how our data is distributed 
hist(X,breaks=floor(sqrt(B)),freq=F)

# and compare it to Real Exponential distribution (PDF), with lambda = 0.1
Y <- rexp(1000,0.1)
hist(Y,breaks=floor(sqrt(B)),freq=F, main = "Exponential(0.1) PDF")
# We can see that they are both very close. 

# ECDF of our data
Fn <- ecdf(X)
summary(Fn)
plot(Fn, xlab = "Sample quantiles from lifetime" , main="Empirical Cumulative Distribution of lifetime, X")

# STEP 2) We use Kolomogrov-Smirnov Goodness of Fit Test to confirm our hypothesis, testing our data
# against exponential, where lambda is it's MLE estimate, 1/xbar 
ks.test(X, pexp, rate = 1/lambda.hat)

# STEP 3) Do a qq plot of the two distributions. 
qqnorm(X)
qqline(X)

#########################################
######        Course: STA312       ######
###### Final Project: Question 3D  ######
###### Sant Saurabh   | 1002434047 ######
#########################################

# Here, we do a parametric Bootstrap, with exponential density
# as our parametric distribution, to find the Median & Kurtosis of lifetime. 

# We are asked to find the parametric bootstrapped median.
# Parametric distribution is the same as non-parametric, except that we sample
# from the distribution. 

#############################
####  Parametric Median  ####
#############################

# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size is 1000
boot.par <- NULL        

# Parametric Bootstrap Estimate of the Median
for(i in 1:B){
  Par.bootstrap <- NULL          # Storage for each sample
  Par.bootstrap <- rexp(n, rate=lambda)
  Par.Median <- median(Par.bootstrap)
  boot.par <- c(boot.par, Par.Median)
}
hist(boot.par,breaks=floor(sqrt(B)),freq=F, main="Histogram of Parametric Bootstrapped Median")

###############################
####  Parametric Kurtosis  ####
###############################

# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size is 1000
boot.par <- NULL        

# Parametric Bootstrap Estimate of the Median
for(i in 1:B){
  Par.bootstrap <- NULL  # Storage for each sample
  Par.bootstrap <- rexp(n, rate=lambda)
  Par.mean <- mean(Par.bootstrap)
  Par.var <- var(Par.bootstrap)
  Par.kurtosis <- ( sum((Par.bootstrap - (Par.mean))^4 )/n ) / (Par.var^2)
  boot.par <- c(boot.par, Par.kurtosis)
}
hist(boot.par, breaks=floor(sqrt(B)),freq=F, main="Histogram of Parametric Bootstrapped Kurtosis")

###################################
####  Parametric 95% Quantile  ####
###################################

# Set up the bootstrap
B <- 1000               # number of replicates
n <- length(X)          # sample size is 1000
boot.par <- NULL        

# Parametric Bootstrap Estimate of the Median
for(i in 1:B){
  Par.bootstrap <- NULL          # Storage for each sample
  Par.bootstrap <- rexp(n, rate=lambda)
  Par.Quantile <- quantile(Par.bootstrap, 0.95)
  boot.par <- c(boot.par, Par.Quantile)
}
hist(boot.par, breaks=floor(sqrt(B)),freq=F, main="Histogram of Parametric 95% Quantile")

# The 95% Quantile Estimate is:
print(quantile(boot.par,0.95))

# The distribution looks approximately normal, thus we quickly check it:
qqnorm(boot.par)
hist(boot.par)
boxplot(boot.par)

# Do KS Test
x <- boot.par
# Standard normal
y <- rnorm(1000)
ks.test(x, y)

# Pvalue is very low, thus we reject H0 that is from the same distribution and
# we cannot assume normality, even though the plots make it appear it is normal. 

# Thus, we provide the 95%  percentile CI
print(c(quantile(boot.par,0.025), quantile(boot.par,0.975)))


#########################################
######        Course: STA312       ######
###### Final Project: Question 3E  ######
###### Sant Saurabh   | 1002434047 ######
#########################################

# BOOTSTRAPPED PARAMETRIC VALUES

# BOOTSTRAPPED NON-PARAMETRIC VALUES

# THEORETICAL VALUES
##############################################
####  BOOTSTRAPPED NON-PARAMETRIC VALUES  ####
##############################################

##########################################
####  T  ####
##########################################

###################################
####  Parametric 95% Quantile  ####
###################################

