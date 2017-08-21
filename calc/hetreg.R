rm(list=ls())
library(MASS)
library(foreign)


##############################################
# simulate data for heteroskedastic regression

# 1) set up the beta & sigma values
beta0 <- 1.5
beta1 <- 2
beta2 <- -1
sigma0 <- -0.5
sigma1 <- -0.1

# 2) number of observations (and num. of Simulations)
nobs = 1000

# 3) set up covariates
x1 <- exp(rnorm(nobs,0,1))
x2 <- runif(nobs,0,3)
z1 <- rnorm(nobs,0,2)

# 4) Calculate Y
gamma <- exp(sigma0 + sigma1 * z1 + rnorm(nobs,mean=0,sd=0.5))
y <- NULL

for(i in 1:nobs){
  y[i] <- beta0 + x1[i] * beta1 + x2[i] * beta2 + rnorm(1,mean=0,sd=gamma[i])
}


############
# check data

par(mfrow=c(2,2))
plot(density(y))
plot(y~x1)
plot(y~x2)
plot(y~z1)
par(mfrow=c(1,1))

y <- as.matrix(y)
x <- cbind(x1,x2)
z <- as.matrix(z1)



##################################
# Heteroskedastic regression in R

hetero.lik <- function(theta, y, x, z) {
  x <- cbind(1,x)
  z <- cbind(1,z)
  beta <- theta[1:ncol(x)]
  gamma <- theta[(ncol(x)+1):(ncol(x)+ncol(z))]
  # Residuals
  e <- y - (x%*%beta)
  # variance parameterization
  sigma2 <- exp(z%*%gamma)
  # Log lik function for each observation
  logl <- -1/2*log(sigma2) - 1/2*(e^2/(sigma2))
  # Log lik function is sum over N observations
  logl <- sum(logl)
  return(logl)
}

# start values for maximization algorithm
stval <- rep(0,ncol(x)+ncol(z)+2)
# maximize the likelihood function numerically using optim()
res <- optim(stval                       # starting values
             , fn=hetero.lik             # the likelihood function
             , method="BFGS"             # maximization method
             , control=list(fnscale=-1, trace=1, maxit=200)  # maximize rather than minimize funct
             , y=y, x=x, z=z             # the data
             , hessian=T)

# results in table
result <- cbind(res$par,sqrt(diag(solve(-res$hessian))))
colnames(result) <- c("Coef","S.E.")
rownames(result) <- c(paste("beta",0:ncol(x)), paste("sigma",0:1))
print(result)

# R Results for Sigma coefficients
result[4:5,1]

