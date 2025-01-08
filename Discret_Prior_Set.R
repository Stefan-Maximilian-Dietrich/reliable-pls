normal_radnom_spaced <- function(n, a,b){
  sigma_priori <- diag(length(a))
  mu <- list()
  
  for(i in 1:n){
    mu_priori <- c()
    for(j in 1:length(a)) {
      mu_priori <- c(mu_priori, runif(1, min = a[j], max = b[j]))
    }
    mu[[i]] <- mu_priori
  }

  
  FUN <- function(mu) {
    return( function(theta) { mvnfast::dmvn(X = theta, mu = mu, sigma = sigma_priori)})
  }
  density <- lapply(mu, FUN)

  return(density)
}

likelihood_logistic <- function(X, response) {
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
  
  return(likelihood)
}


marginal_likelyhood <- function(likelihood, priori) {
  g <- function(x) {
    return(likelihood*priori)
  }
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  start <- rep(0, times = ncol(X)) #Starting point 
  x0 <- optim(par = start, fn = h_neg, method = "BFGS")$par  #Similar to the Newton method.
  hII_x0 <- hessian(h, x0) #Calculation of the Hessian matrix (i.e., the second derivative with respect to the point x0).
  
  marginal_likelyhood <- exp( h(x0) ) * sqrt( (2*pi)^2 / abs(det(hII_x0))) #Laplace Approximation
  return(marginal_likelyhood)
}

alpha_cut <- function() {
  
}


####################### TEST ##################################################

 A <- likelihood_function()