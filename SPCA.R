#########################################
######        Course : STA315      ######
###### Final Project : Zou' Results######
######    Stanley Wan | 1002396276 ######
######  Sant Saurabh | 1002434047  ######
#########################################



#########################################
### Simulating example 5.2 from paper ###
#########################################

# Installing and loading the relevant package
#install.packages("elasticnet")
library("elasticnet")

# 100 Monte Carlo runs
n <- 100
nMC <- 100

set.seed(2020)
# Defining the factors 
err <- rnorm(n, 0, sd = 1)
v1 <- rnorm(n, 0, sd = 290)
v2 <- rnorm(n, 0, sd = 300)
v3 <- -0.3*v1 + 0.925*v2 + err

# Defining the observational variables
err1 <- rnorm(n, 0, sd = 1)
err2 <- rnorm(n, 0, sd = 1)
err3 <- rnorm(n, 0, sd = 1)
x1 <- v1 + err1
x2 <- v1 + err1
x3 <- v1 + err1
x4 <- v1 + err1

x5 <- v2 + err2
x6 <- v2 + err2
x7 <- v2 + err2
x8 <- v2 + err2

x9 <- v3 + err3
x10 <- v3 + err3

# putting all variables into matrix form 
x <- data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

# fitting the SPCA model
spc1 <- spca(x=x, 2, para=c(4,4), type="predictor", sparse = "varnum")
spc1

# fitting the PCA model
pca <- prcomp(x, center = TRUE, rank. = 3)
pca
