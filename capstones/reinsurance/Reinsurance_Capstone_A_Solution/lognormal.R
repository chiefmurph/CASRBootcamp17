# lognormal.r
# LOGNORMAL DISTRIBUTION UTILITY FUNCTIONS
##    Copyright (C) <2012>  <Daniel Murphy>
# Open source
# License: GPL (>= 2)
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Return the expected value
lnormMean  <- function(mu, sigma) exp(mu + sigma^2 / 2)
# Return the standard deviation
lnormStd   <- function(mu, sigma) sqrt((exp(sigma^2) - 1)) * lnormMean(mu, sigma)
# Return the coefficient of variation
lnormCv    <- function(mu, sigma) lnormStd(mu, sigma) / lnormMean(mu, sigma)
# Return the sigma parameter of the distribution given the mean and std dev
lnormSigma <- function(mean, std) {
  if (any(mean < 0) | any(std < 0)) stop("Negative mean or std")
  sqrt(log(1 + (std / mean)^2))
  }
# Return the mu parameter of the distribution given the mean and std dev
lnormMu    <- function(mean, std) {
  if (any(mean < 0) | any(std < 0)) stop("Negative mean or std")
  log(mean) - .5 * log(1 + (std / mean)^2)
  }
# return the vector (mu, sigma) of parameters of the distribution given the mean and std dev
# 9/2/2014: return a matrix if length(mu) and sigma > 1
lnormParms <- function(mean, std){
  stopifnot(length(mean) == length(std))
  if (any(mean < 0) | any(std < 0)) stop("Negative mean or std")
  sigma2 <- log(1 + (std / mean)^2)
  mu     <- log(mean) - .5 * sigma2
  cbind(mu = mu, sigma = sqrt(sigma2))[,]
  }
lnormParmsListVersion <- function(mean, std){
  if (any(mean < 0) | any(std < 0)) stop("Negative mean or std")
  sigma2 <- log(1 + (std / mean)^2)
  mu     <- log(mean) - .5 * sigma2
  list(mu    = mu,
       sigma = sqrt(sigma2))
  }

# return random deviates given the mean and variance of the distribution
rlnorm.meanvar <- function(n, mean=exp(.5), var=exp(1)*(exp(1)-1))
  rlnorm(n, log(mean)-.5*(sigma2<-log(1+var/mean^2)), sqrt(sigma2))
# return random deviates given the mean and std dev of the distribution
rlnorm.meanstd <- function(n, mean=exp(.5), std=sqrt(exp(1)*(exp(1)-1)))
  rlnorm(n, log(mean)-.5*(sigma2<-log(1+(std/mean)^2)), sqrt(sigma2))
# return quantiles given the mean and variance of the distribution
qlnorm.meanvar <- function(p, mean=exp(.5), var=exp(1)*(exp(1)-1))
  qlnorm(p, log(mean)-.5*(sigma2<-log(1+var/mean^2)), sqrt(sigma2))
# return quantiles given the mean and std dev of the distribution
qlnorm.meanstd <- function(p, mean=exp(.5), std=sqrt(exp(1)*(exp(1)-1)))
  qlnorm(p, log(mean)-.5*(sigma2<-log(1+(std/mean)^2)), sqrt(sigma2))
                      
    # Wikipedia: If X~lognormal, then X + c is called shifted log-normal.

rbvlnorm<-function(n,mn1,std1,mn2,std2,rho){
# Generate random draws from bivariate lognormal distribution
# with marginal means mn1, mn2, standard deviations std1, std2,
# and average correlation rho
#
# Algorithm: find the corresponding parameters for bivariate normal
# such that when random draws from that multivariate normal 
# are exponentiated, the resulting multivariate lognormals 
# will have the desired properties.
  # parameters of normals corresponding to lognormal marginals
  parms<-lnorm.parms(mn1,std1)
  mu1<-parms[1];s1<-parms[2]
  parms<-lnorm.parms(mn2,std2)
  mu2<-parms[1];s2<-parms[2]

  # Calculate corresponding normal correlation parameter
  # see, e.g., Johnson, N. L. and Kotz, S. (1972). 
  # "Distributions in Statistics: Continuous Multivariate
  #  Distributions", New York, Wiley., p.20
  rhoN=log(rho*sqrt((exp(s1^2)-1)*(exp(s2^2)-1))+1)/s1/s2

  # Prepare covariance matrix for call to builtin function
  sigma <- matrix( c(s1^2,cov<-s1*s2*rhoN,cov,s2^2), ncol=2)   
  return(exp(rmvnorm(n, mean=c(mu1,mu2), sigma=sigma)))
  }

# Solutions for Multivariate parameters
# http://stackoverflow.com/questions/7663690/log-covariance-to-arithmetic-covariance-matrix-function
# http://r.789695.n4.nabble.com/multivariate-lognormal-distribution-simulation-in-compositions-td4646140.html
#Untested
lmvnormParms <- function(Mu, Sigma) {
  # Mu = mu parameter of the lognormals
  # Sigma = 
  m <- exp(Mu + diag(Sigma) / 2) - 1
  x1 <- outer(Mu, Mu, "+")
  x2 <- outer(diag(Sigma), diag(Sigma), "+") / 2
  S <- exp(x1 + x2) * (exp(Sigma) - 1)
  list(Mu = m, Sigma = S)
}
logreturn <- function(am,asigma) {
  M <- 1/(1+am)
  S <- log( diag(M) %*% asigma %*% diag(M) + 1)
  mu <- log(1+am) - diag(S)/2
  list(mean=mu, vcov=S)
}
