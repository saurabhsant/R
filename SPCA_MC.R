#########################################
######        Course : STA315      ######
###### Final Project : MC Results  ######
######    Stanley Wan | 1002396276 ######
######  Sant Saurabh | 1002434047  ######
#########################################

#########################################
##### MC Simulatiion 5.2 from paper #####
#########################################


# Installing and loading the relevant package
#install.packages("elasticnet")
library("elasticnet")

# 100 Monte Carlo runs
n <- 100
nMC <- 100

beta_mc_spca <- rep(0,10)
beta_mc_spca2 <- rep(0,10)

beta_mc_pca <- rep(0,10)
beta_mc_pca2 <- rep(0,10)

beta_hat_spca <- matrix(rep(0, nMC*10), ncol = 100, nrow = 10)
beta_hat_spca2 <- matrix(rep(0, nMC*10), ncol = 100, nrow = 10)

beta_hat_pca <- matrix(rep(0, nMC*10), ncol = 100, nrow = 10)
beta_hat_pca2 <- matrix(rep(0, nMC*10), ncol = 100, nrow = 10)
#set.seed(2004)

for (i in 1:nMC){
  set.seed(2018+100*i)	
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
  spc1 <- spca(x=x, 2, para=c(4, 4), type="predictor", sparse = "varnum")
  beta_hat_spca[,i] <- spc1$loadings[1:10]
  beta_hat_spca2[,i] <- spc1$loadings[11:20]
  
  pca1 <- prcomp(x, center = TRUE)
  beta_hat_pca[,i] <- pca1$rotation[1:10]
  beta_hat_pca2[,i] <- pca1$rotation[11:20]
}

# Estimate beta by MC
for( i in 1:length(beta_mc)){
  beta_mc_spca[i] <- mean(beta_hat_spca[i,])
  beta_mc_spca2[i] <- mean(beta_hat_spca2[i,])
  
  beta_mc_pca[i] <- mean(beta_hat_pca[i,])
  beta_mc_pca2[i] <- mean(beta_hat_pca2[i,])
}
# Estimating Loadings by SPCA using Monte Carlo
round(beta_mc_spca, 3)
round(beta_mc_spca2, 3)
# Calculating MSE for SPCA
mse_spca_mc <-sum( (beta_hat_spca - matrix(rep(beta,100), ncol=100))^2 ) / nMC
mse_spca2_mc <-sum( (beta_hat_spca2 - matrix(rep(beta,100), ncol=100))^2 ) / nMC
print(round(c(mse_spca_mc, mse_spca2_mc), 3))

# Estimating Loadings by PCA using Monte Carlo
round(beta_mc_pca, 3)
round(beta_mc_pca2, 3)
# Calculating MSE for PCA
mse_pca_mc <-sum( (beta_hat_pca - matrix(rep(beta,100), ncol=100))^2 ) / nMC
mse_pca2_mc <-sum( (beta_hat_pca2 - matrix(rep(beta,100), ncol=100))^2 ) / nMC
print(round(c(mse_pca_mc, mse_pca2_mc), 3))