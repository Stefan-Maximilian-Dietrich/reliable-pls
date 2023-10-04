
log_likelihood_mu_function <- function(beta1, data) {
  x <- as.numeric(data[,1])
  y <- as.numeric(data[,2])
  
  logistic_function <- function(x, beta0, beta1) {
    odds <- exp(beta0 + beta1 * x)
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  log_likelihood <- function(beta0, beta1) {
    p <- logistic_function(x, beta0, beta1)
    log_lik <- sum(y * log(p) + (1 - y) * log(1 - p))
    return(log_lik)
  }
  
  log_likelihood_mu1 <- function(beta0) {
    result <- log_likelihood(beta0 = beta0 , beta1 = beta1)
    return(result)
  }
  
  result <- optim(par = 0, fn = log_likelihood_mu1, control = list(fnscale = -1), method = "BFGS")
  
  return(result$value)
  
}

data <- mtcars[c(1,8)]
beta1 <- 1

log_likelihood_mu_function(1, data) 

log_likelihood_mu_function_test <- function(beta1) {
  return(log_likelihood_mu_function(beta1, data=data)) 
}


###############################################################################


log_likelihood_mu_function2 <- function(beta1, data) {
  x <- as.numeric(data[,1])
  y <- as.numeric(data[,2])
  
  logistic_function <- function(x, beta0, beta1) {
    odds <- exp(beta0 + beta1 * x)
    probability <- odds / (1 + odds)
    print(odds)
    return(probability)
  }
  
  log_likelihood <- function(beta0, beta1) {
    p <- logistic_function(x, beta0, beta1)
    log_lik <- sum(y * log(p) + (1 - y) * log(1 - p))
    return(log_lik)
  }
  
  beta0 <- 0
  
  result <- log_likelihood(beta0 = beta0, beta1 = beta1)

  return(result)
  
}

data <- mtcars[c(1,8)]


log_likelihood_mu_function2(1.1, data) 

log_likelihood_mu_function_test2 <- function(beta1) {
  return(log_likelihood_mu_function2(beta1, data=data)) 
}
