# Question 1, Assignment 1
# Last Name: Sant, First Name: Saurabh
# St. #: 1002434047
# function to generate from a Poisson distribution
simPoisson <- function(n,lambda){
  urandom <- runif(n)
  sim.vector <- rep(0,n)
  for(j in 1:n){
    i <- 0
    p <- (exp((-1)*lambda))
    F <- p
    while(urandom[j] >= F){
      p <- lambda*p/(i+1)
      F <- F+p
      i<- i+1
    }
    sim.vector[j] <- i
  }
  # output
  sim.vector
}
n=1000
lambda = 2
output1 <- simPoisson (n,lambda)
sim.mean=mean(output1)
sim.mean; var(output1)



# Question 2c, Assignment 1
# Last Name: Sant, First Name: Saurabh
# St. #: 1002434047
n <- 1000
j <- 20 # number of X_i
y <- NULL

for (i in 1:n){
  x = rexp(j,1)
  y[i] = max(x)
}
mean.sample = mean(y)
h <- hist(y, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, freq= TRUE, main = "Q2c: Relative Frequency Histogram")



# Question 3b, Assignment 1
# Last Name: Sant, First Name: Saurabh
# St. #: 1002434047
n <- 1000
x = numeric(n)

for (i in 1:n){
  u = runif(12, -0.5, 0.5)
  x[i] = sum(u)
}
mean(x);var(x)
h <- hist(x, plot=F)
h$counts <- h$counts/sum(h$counts)
plot(h, freq= TRUE, main = "Q3b: Relative Frequency Histogram")



# Question 4c, Assignment 1
# Last Name: Sant, First Name: Saurabh
# St. #: 1002434047
n <- 1000
k <- 0  #counter for accepted
j <- 0  #iterations
x <- numeric(n)

while (k < n){
  u <- runif(1)
  j <- j + 1
  y <- rnorm(1)

  if (u < 0.2*((sin(6*y))^2 + 3*((cos(y))^2)*((sin(4*y))^2) + 1)) {
    k <- k + 1
    x[k] <- y
  }
}
mean.sample = mean(x)
var.sample = var(x)
mean.sample; var.sample

h<-hist(x, plot=F)
h$counts <- h$counts / sum(h$counts)
plot(h, freq=TRUE, ylab="Q4c: Relative Frequency Histogram")

