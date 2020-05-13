par(mfrow=c(1,2))
# Question 4d
n <- 1000
u <- runif(n)
u_2 <- u^2
hist(u_2, freq=F, xlab="U^2", main="Generating Beta(0.5, 1) RV by squaring Uniform")
curve((dunif(x, 0, 1, log=FALSE))^2, 0, 3, lwd=2, xlab= "", ylab="", add = T, col="red")


# Question 4e
n <- 1000
u2 <- rbeta(n,0.5,1,ncp=0)
hist(u2, freq=F, xlab="U^2", main="Generating Beta(0.5, 1) RV using rbeta")
curve(dbeta(x, 0.5, 1), 0, 3, lwd=2, xlab= "", ylab="", add = T, col="red")


# Question 5e
n <- 1000
u <- runif(n)

for (i in u){
  if (i<0.5){
    X <- -log(2-2*u)
    y1 <- 0.5*exp(u)
    
    
  } else if (i>0.5){
    Y <- log(2*u)
    y2 <- 1 - 0.5*exp(-u)
    
  }
}
hist(X, freq=F, xlab= "X",ylim=c(0,0.8), xlim=c(-5,5), main="Generating X")
hist(Y, freq=F, xlab= "X", add = T)
lines(density(y1), add = T)
lines(density(y2), add = T)

# Question 6f)
sigma <- c(1, 2, 3, 4)
n <- 10000
par(mfrow = c(2,2))
sample.mean=c(NA,NA,NA,NA)


for (i in 1:(length(sigma))) {
  u <- runif(n)
  X <- sqrt(-2*(i^2)*log(1-u))
  sample.mean[i]=mean(X)
  hist(X, prob=TRUE, main=sprintf("Sigma = %s", sigma[i]))
  curve(drayleigh(x, scale = sigma[i], log = FALSE), lwd=2,xlab="", add=T)
}
exact.mean=sigma*sqrt(pi/2)

