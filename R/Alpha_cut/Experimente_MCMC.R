##############################################################
# Comparison of ML for two regression models
install.packages("BayesianTools")
library(BayesianTools)

n =400
A <- rnorm(n, mean = 0.2)
B <- rnorm(n, mean = -2)

param <- c(A,B)

lin_comb <- 2.4- 7.9*param[1] + 0.5*param[2] 

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)

likelihood_function <- function(X, theta, response) {
  Y <- response
  
  logistic_function <- function(X, theta) {
    odds <- exp(X %*% theta) #Odds
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    likelihood <- prod(Y * p + (1 - Y) * (1 - p))
    return(likelihood)
  }
  result <- likelihood(theta = theta)
  return(result)
}


# likelihoods for linear and quadratic model 
likelihood1 <- function(param){
  prob = 1/(1+exp(-(2.4- 7.9*param[1] + 0.5*param[2])))
  singlelikelihoods = dbinom(target_var, size =1, prob = prob, log = TRUE)
  return(sum(singlelikelihoods))  
}


density = function(par){
  d1 =  dmnorm(x = c(par[1], par[2]), mean = c(0,0), varcov = diag(2), log = TRUE)
  return(d1)
}
sampler = function(n=1){
  d1 = rmnorm(n, mean = c(0,0), varcov = diag(2))

  return(d1)
}
prior1 <- createPrior(density = density, sampler = sampler,
                     lower = c(-10,-20), upper = c(10,20), best = NULL)


prior$density(c(2, 3))
dmnorm(x = c(c(2, 3)), mean = c(0,0), varcov = diag(2))

setUp1 <- createBayesianSetup(likelihood1, 
                              prior =  prior1)


out1 <- runMCMC(bayesianSetup = setUp1)
M1 = marginalLikelihood(out1, start = 1000)
exp(M1$ln.ML)

out2 <- runMCMC(bayesianSetup = setUp2)
M2 = marginalLikelihood(out2, start = 1000)
exp(M2$ln.ML)

# note that the createPrior supports additional truncation
# To use a prior in an MCMC, include it in a BayesianSetup