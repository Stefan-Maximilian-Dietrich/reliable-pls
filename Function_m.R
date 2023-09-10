install.packages("numDeriv")
install.packages("mvtnorm")
install.packages("pracma")
library(numDeriv)
library(mvtnorm)
library(ggplot2)
library(pracma)


product_function <- function(data, mean, sigma){
  product <- 1
  for(j in seq(length(data))) {
    product <- product * dmvnorm(x  = unlist(data[j]), mean = mean, sigma = matrix(sigma))
  }
  return(product)
}

likelihood_function <- function(data, theta_mu, theta_sigma) {
  likelihood <- product_function(data, mean = theta_mu, sigma = theta_sigma)
  print(likelihood)
  return(likelihood)
}

#################################################################################

priori_function <- function(theta, mu_priori, sigma_priori) {
  priori <- dmvnorm(x = theta, mean = mu_priori, sigma = matrix(sigma_priori)) #hier wirs nur die wahrscheilichekit des Mittelwerts betrachtet nicht die der sTREUUNG
  return(priori)
}

m_derivat_function <- function(data, mu_priori, sigma_priori, theta_mu, theta_sigma) {
  m_derivat <- likelihood_function(data, theta_mu, theta_sigma) * priori_function(theta_mu, mu_priori, sigma_priori)
  return(m_derivat)
}

m_mu_functio <- function(data, mu_priori, sigma_priori, theta_sigma) {
  g <- function(x) {
    return(m_derivat_function(data = data, mu_priori = mu_priori, sigma_priori = sigma_priori, theta_mu = x, theta_sigma = theta_sigma))
  }
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  
  x0 <- optim(par = 0, fn = h_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  hII_x0 <- fderiv(f = h, x = x0, n = 2, h = 5*10^-6)
  
  m_mu <- exp( h(x0) ) * sqrt( (2*pi) / -hII_x0) # * (pnorm(b, mean = x0, sd = sqrt(-1 / hII_x0)) - pnorm(a, mean = x0, sd = sqrt(-1 / hII_x0))) falls Parameter eingeschränkt 
    
  return(m_mu)
} 

root_function <- function(f, intervall, step) {
  roots <- c()
  seq <- seq(from = intervall[1], to = intervall[2], by = step)
  for(i in seq(length(seq)-1)) {
    if(f(seq[i]) * f(seq[i+1]) < 0 ) {
      inter <- c(seq[i], seq[i+1])
      roots <- c(roots, uniroot(f, inter)$root)
    }
  }
  return(roots)
}

AlphaCut_mu_function <- function(alpha, mu_priori_Lower, mu_priori_upper, data, sigma_priori, theta_sigma) {
  m_mu_log_neg <- function(x) {
    return(-log(m_mu_functio(data = data, mu_priori = x, sigma_priori = sigma_priori, theta_sigma = theta_sigma)))
  }
  max_par <- optim(par = 0, fn = m_mu_log_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  max_m <- m_mu_functio(data = data, mu_priori = max_par, sigma_priori = sigma_priori, theta_sigma = theta_sigma)
 
  f <- function(x) {
    return(m_mu_functio(data = data, mu_priori = x, sigma_priori = sigma_priori, theta_sigma = theta_sigma) - alpha * max_m)
  }
  
  intervall <- c(mu_priori_Lower, mu_priori_upper)
  step = 0.1
  
  root <- root_function(f, intervall, step)
  return(root)
}

utility_function <- function(a, labeld_data, unlabeled_data, theta_mu, theta_sigma, mu_priori, sigma_priori) {
  new_data <- c(labeld_data, unlabeled_data[a])
  fischer_info <-  - 0.25 * log(det(matrix(var(new_data))))
  log_likelihood <- log(likelihood_function(data = new_data, theta_mu = theta_mu, theta_sigma = theta_sigma))
  para_obs <- 1/2 * log(2*pi/length(new_data))
  log_priori <- log(priori_function(theta = theta_mu, mu_priori = mu_priori, sigma_priori = sigma_priori))
  
  print(fischer_info)
  print(log_likelihood)
  print(para_obs)
  print(log_priori)
  

  return(log_likelihood + para_obs + fischer_info + log_priori)
}

expected_utility_function <- function(a, labeld_data, unlabeled_data, theta_sigma, mu_priori, sigma_priori) {
  g <- function(x) {
    return(utility_function(a = a, labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = x, theta_sigma = theta_sigma, mu_priori = mu_priori, sigma_priori = sigma_priori))
  }
  
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  
  x0 <- optim(par = 20, fn = h_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  hII_x0 <- fderiv(f = h, x = x0, n = 2, h = 5*10^-6)
  
  expected_utility <- exp( h(x0) ) * sqrt( (2*pi) / -hII_x0) # * (pnorm(b, mean = x0, sd = sqrt(-1 / hII_x0)) - pnorm(a, mean = x0, sd = sqrt(-1 / hII_x0))) falls Parameter eingeschränkt 
  
  return(expected_utility)
}

gamma_maximin_function <- function(a, labeld_data, unlabeled_data, theta_sigma, mu_priori_lower, mu_priori_upper, sigma_priori) {
  
}
