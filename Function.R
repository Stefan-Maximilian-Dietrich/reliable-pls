install.packages("numDeriv")
install.packages("mvtnorm")
install.packages("pracma")
library(numDeriv)
library(mvtnorm)
library(ggplot2)
library(pracma)
library(nloptr)
library(tidyverse)

# n dim 
################################################################################
likelihood_function <- function(theta, data_matrix, response) {
  X <- as.matrix(cbind(1, data_matrix))
  Y <- as.matrix(response)
  
  logistic_function <- function(X, theta) {
    odds <- exp(X %*% theta)
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    likelihood <- prod(ifelse(Y == 1, p, 1 - p))
    return(likelihood)
  }
  
  result <- likelihood(theta = theta)
  
  return(result)
}

priori_function <- function(theta, mu_priori, sigma_priori) {
  priori <- dmvnorm(x = theta, mean = mu_priori, sigma = sigma_priori) #hier wirs nur die wahrscheilichekit des Mittelwerts betrachtet nicht die der sTREUUNG
  return(priori)
}

utility_approximation_function <- function(data_matrix, response, theta, mu_priori, sigma_priori) {
  fischer_info <-  sqrt((det(as.matrix(var(data_matrix)))))
  likelihood <- likelihood_function(theta = theta, data_matrix = data_matrix,  response = response)
  para_obs <- 2*pi/nrow(data_matrix)
  priori <- priori_function(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return(para_obs * likelihood * priori / fischer_info)
}

expected_utility_function <- function(data_matrix, response, mu_priori, sigma_priori) {

  f <- function(x) {
    expected_utility <- utility_approximation_function(data_matrix = data_matrix, response = response, theta = x, mu_priori = mu_priori, sigma_priori = sigma_priori)
    return(expected_utility)
  }
  h <- function(x) {
    return(-log(f(x)))
  }
  
  start <- rep(0, times = ncol(data_matrix) + 1)
  arg_max_likelihood <- optim(par = start, fn =  h, method = "BFGS")$par
  
  result <- utility_approximation_function(data_matrix = data_matrix, response = response, theta = arg_max_likelihood, mu_priori = mu_priori, sigma_priori = sigma_priori)

  print(paste("mu_priori_b0:", format(round(mu_priori[1], 5), nsmall = 5),"mu_priori_b1:", format(round(mu_priori[2], 5), nsmall = 5), "mu_priori_b2:", format(round(mu_priori[3], 5), nsmall = 5),"  e_utility:", result))
  #print(paste("mu_priori_b0:", format(round(mu_priori[1], 5), nsmall = 5),"mu_priori_b1:", format(round(mu_priori[2], 5), nsmall = 5), "  e_utility:", result))
  
  
  return(result)
} 

m_derivat_function <- function(data_matrix, response, mu_priori, sigma_priori, theta) {
   m_derivat <- likelihood_function(theta = theta, data_matrix = data_matrix, response = response) * priori_function(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  return(m_derivat)
}

m_mu_function <- function(data_matrix, response, mu_priori, sigma_priori) {
  
  g <- function(x) {
    return(m_derivat_function(data_matrix = data_matrix, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori, theta = x))
  }
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  
  start <- rep(0, times = ncol(data_matrix) + 1)
  x0 <- optim(par = start, fn = h_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  
  hII_x0 <- hessian(h, x0)

  m_mu <- exp( h(x0) ) * sqrt( (2*pi)^2 / abs(det(hII_x0))) # * (pnorm(b, mean = x0, sd = sqrt(-1 / hII_x0)) - pnorm(a, mean = x0, sd = sqrt(-1 / hII_x0))) falls Parameter eingeschränkt 
  return(m_mu)
} 

m_alpha_function <- function(data_matrix , response , mu_priori , sigma_priori, alpha) {
  
  fn <- function(x) {
    result <- - m_mu_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori) 
    return(result)
  }
  
  x0 <- rep(0, ncol(data_matrix) + 1)
  m_max <- - nloptr(x0 = x0, eval_f = fn, opts = list("algorithm"="NLOPT_LN_COBYLA"))$objective
  
  result <-  m_mu_function(data_matrix = data_matrix, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori) - m_max * alpha
  
  return(result)
}

gamma_maximin_alpaC_function <- function(data_matrix, response, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  expected_utility <- function(x) {
    result <- expected_utility_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(result)
  }
  
  m_alpha <- function(y) {
    result <- - m_alpha_function(data_matrix = data_matrix , response = response, mu_priori = y, sigma_priori = sigma_priori, alpha = alpha)
    return(result)
  }
  
  x0 <- (mu_priori_upper + mu_priori_lower)/2
  result <- nloptr(x0=x0, eval_f = expected_utility, lb = mu_priori_lower, ub = mu_priori_upper, eval_g_ineq = m_alpha, opts = list("algorithm"="NLOPT_LN_COBYLA"))
  
  return(result)
}

gamma_maximin_alpaC_addapter <- function(data, glm_formula, target, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  variables <- all.vars(glm_formula)
  pred_variables <- variables[variables != target]
  data_matrix <- as.matrix(selected_column <- subset(data, select = pred_variables))
  response <- as.matrix(selected_column <- subset(data, select = target))
  
  result <- gamma_maximin_alpaC_function(data_matrix = data_matrix, response = response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
  return(result)
}

gamma_maximin_function <- function(data_fram, target, mu_priori_lower, mu_priori_upper, sigma_priori) {
  data_matrix <-  as.matrix(data_fram[, !colnames(data_fram) %in% target])
  response <- as.matrix(data_fram[, target]) 
  start <- (mu_priori_upper - mu_priori_lower)/2
  f <- function(x) {
    expected_utility <- expected_utility_function(data_matrix = data_matrix , response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(expected_utility)
  }
  parameter <- optim(par = start, fn = f, method = "L-BFGS-B", lower = mu_priori_lower, upper = mu_priori_upper)$par
  result <- f(parameter)
  return(result)
}

decision_function <- function(labeld_data, unlabeled_data, target,  mu_priori_lower, mu_priori_upper, sigma_priori) {
  labeld_data_matrix <- as.matrix(labeld_data[, !colnames(labeld_data) %in% target])
  unlabeled_data_matrix <- as.matrix(unlabeled_data[, !colnames(labeld_data) %in% target])
  response <- as.matrix(labeld_data[c(target)])
  pseudo_response <- as.matrix(unlabeled_data[c(target)])
  
  n_unlabeld <- nrow(unlabeled_data)
  results <- c()
  for(i in seq(n_unlabeld)) {
    results_new <- gamma_maximin_function(a = i, labeld_data_matrix = labeld_data_matrix, unlabeled_data_matrix = unlabeled_data_matrix, response = response, pseudo_response = pseudo_response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori)
    results <- c(results, results_new)
  }
  decision <- which.max(results)
  print(unlabeled_data[decision,])
  return(decision)
}

decision_function <- function(labeld_data, unlabeled_data, target,  mu_priori_lower, mu_priori_upper, sigma_priori) {
  labeld_data_matrix <- as.matrix(labeld_data[, !colnames(labeld_data) %in% target])
  unlabeled_data_matrix <- as.matrix(unlabeled_data[, !colnames(labeld_data) %in% target])
  response <- as.matrix(labeld_data[c(target)])
  pseudo_response <- as.matrix(unlabeled_data[c(target)])
  
  n_unlabeld <- nrow(unlabeled_data)
  results <- c()
  for(i in seq(n_unlabeld)) {
    results_new <- gamma_maximin_function(a = i, labeld_data_matrix = labeld_data_matrix, unlabeled_data_matrix = unlabeled_data_matrix, response = response, pseudo_response = pseudo_response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori)
    results <- c(results, results_new)
  }
  decision <- which.max(results)
  print(unlabeled_data[decision,])
  return(decision)
}


# 2dim 
################################################################################
likelihood_function <- function(beta0, beta1, data) {
  x <- as.numeric(data[,1])
  y <- as.numeric(data[,2])
  
  logistic_function <- function(x, beta0, beta1) {
    odds <- exp(beta0 + beta1 * x)
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(beta0, beta1) {
    p <- logistic_function(x, beta0, beta1)
    likelihood <- prod(ifelse(y == 1, p, 1 - p))
    return(likelihood)
  }
  
  result <- likelihood(beta0 = beta0, beta1 = beta1)
  
  return(result)
}

priori_function <- function(theta, mu_priori, sigma_priori) {
  priori <- dmvnorm(x = theta, mean = mu_priori, sigma = sigma_priori) #hier wirs nur die wahrscheilichekit des Mittelwerts betrachtet nicht die der sTREUUNG
  return(priori)
}

utility_approximation_function <- function(a, labeld_data, unlabeled_data, theta, mu_priori, sigma_priori) {
  new_data <- rbind(labeld_data, unlabeled_data[a,])
  fischer_info <-  sqrt((det(as.matrix(var(new_data)))))
  likelihood <- likelihood_function(data = new_data, beta0 = theta[1], beta1 = theta[2])
  para_obs <- 2*pi/length(new_data)
  priori <- priori_function(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  

  return(para_obs * likelihood * priori / fischer_info)
}

expected_utility_function <- function(a, labeld_data, unlabeled_data, mu_priori, sigma_priori) {
  f <- function(x) {
    expected_utility <- utility_approximation_function(a = a, labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta = x, mu_priori = mu_priori, sigma_priori = sigma_priori)
    return(expected_utility)
  }
  h <- function(x) {
    return(-log(f(x)))
  }
  arg_max_likelihood <- optim(par = c(0,0), fn =  h, method = "BFGS")$par
  
  result <- utility_approximation_function(a =a, labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta = arg_max_likelihood, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return(result)
} 

gamma_maximin_function <- function(a, labeld_data, unlabeled_data, mu_priori_lower, mu_priori_upper, sigma_priori) {
  start <- (mu_priori_upper - mu_priori_lower)/2
  f <- function(x) {
    expected_utility <- expected_utility_function(a = a, labeld_data =labeld_data , unlabeled_data = unlabeled_data, mu_priori = x, sigma_priori =sigma_priori)
    return(expected_utility)
  }
  result <- optim(par = start, fn = f, method = "L-BFGS-B", lower = mu_priori_lower, upper = mu_priori_upper)
  return(result$value)
}

decision_function <- function(labeld_datam, unlabeled_data, mu_priori_lower, mu_priori_upper, sigma_priori) {
  n_unlabeld <- nrow(unlabeled_data)
  results <- c()
  for(i in seq(n_unlabeld)) {
    results_new <- gamma_maximin_function(a = i, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori)
    results <- c(results, results_new)
  }
  decision <- which.max(results)
  print(unlabeled_data[decision,])
  return(decision)
}

#######

m_derivat_function <- function(data, mu_priori, sigma_priori, theta_mu) {
  print(priori_function(theta_mu, mu_priori, sigma_priori))
  print(likelihood_function(data = data, beta1 = theta_mu))
  m_derivat <- likelihood_function(data = data, beta1 = theta_mu) * priori_function(theta_mu, mu_priori, sigma_priori)
  return(m_derivat)
}

m_mu_function <- function(data, mu_priori, sigma_priori) {
  g <- function(x) {
    return(m_derivat_function(data = data, mu_priori = mu_priori, sigma_priori = sigma_priori, theta_mu = x))
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
    # print(paste("Unten: ", f(seq[i]) , " Obern: " , f(seq[i+1])))
    if(f(seq[i]) * f(seq[i+1]) < 0 ) {
      inter <- c(seq[i], seq[i+1])
      roots <- c(roots, uniroot(f, inter)$root)
    }
  }
  return(roots)
}

AlphaCut_mu_function <- function(alpha, mu_priori_Lower, mu_priori_upper, data, sigma_priori) {
  m_mu_log_neg <- function(x) {
    return(-log(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori)))
  }
  max_par <- optim(par = 0, fn = m_mu_log_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  max_m <- m_mu_function(data = data, mu_priori = max_par, sigma_priori = sigma_priori)
  
  f <- function(x) {
    return(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori) - alpha * max_m)
    print(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori))
    print(alpha * max_m)
  }
  
  intervall <- c(mu_priori_Lower, mu_priori_upper)
  step = 0.01
  
  root <- root_function(f, intervall, step)
  return(root)
}

# 1 Dim
################################################################################

likelihood_function1 <- function(beta1, data) {
  x <- as.numeric(data[,1])
  y <- as.numeric(data[,2])
  
  logistic_function <- function(x, beta0, beta1) {
    odds <- exp(beta0 + beta1 * x)
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(beta0, beta1) {
    p <- logistic_function(x, beta0, beta1)
    likelihood <- prod(ifelse(y == 1, p, 1 - p))
    return(likelihood)
  }
  
  beta0 <-  glm(formula = y ~ x, data = data, family = "binomial")$coefficients[1]
  
  result <- likelihood(beta0 = beta0, beta1 = beta1)
  
  return(result)
  
}

priori_function <- function(theta, mu_priori, sigma_priori) {
  priori <- dmvnorm(x = theta, mean = mu_priori, sigma = matrix(sigma_priori)) #hier wirs nur die wahrscheilichekit des Mittelwerts betrachtet nicht die der sTREUUNG
  return(priori)
}

utility_approximation_function <- function(a, labeld_data, unlabeled_data, theta_mu, mu_priori, sigma_priori) {
  new_data <- rbind(labeld_data, unlabeled_data[a,])
  fischer_info <-  sqrt((det(as.matrix(var(new_data)))))
  likelihood <- likelihood_function(data = new_data, beta1 = theta_mu)
  para_obs <- 2*pi/length(new_data)
  priori <- priori_function(theta = theta_mu, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return(para_obs * likelihood * priori / fischer_info)
}

expected_utility_function <- function(a, labeld_data, unlabeled_data, mu_priori, sigma_priori) {
  f <- function(x) {
    expected_utility <- utility_approximation_function(a = a, labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = x, mu_priori = mu_priori, sigma_priori = sigma_priori)
    return(expected_utility)
  }
  h <- function(x) {
    return(-log(f(x)))
  }
  arg_max_likelihood <- optim(par = 0, fn =  h, method = "BFGS")$par

  result <- utility_approximation_function(a =a, labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = arg_max_likelihood, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return(result)
} 

gamma_maximin_function <- function(a, labeld_data, unlabeled_data, mu_priori_lower, mu_priori_upper, sigma_priori) {
  start <- (mu_priori_upper - mu_priori_lower)/2
  f <- function(x) {
    expected_utility <- expected_utility_function(a = a, labeld_data =labeld_data , unlabeled_data = unlabeled_data, mu_priori = x, sigma_priori =sigma_priori)
    return(expected_utility)
  }
  result <- optim(par = start, fn = f, method = "L-BFGS-B", lower = mu_priori_lower, upper = mu_priori_upper)
  return(result$value)
}

decision_function <- function(labeld_datam, unlabeled_data, mu_priori_lower, mu_priori_upper, sigma_priori) {
  n_unlabeld <- nrow(unlabeled_data)
  results <- c()
  for(i in seq(n_unlabeld)) {
    results_new <- gamma_maximin_function(a = i, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori)
    results <- c(results, results_new)
  }
  decision <- which.max(results)
  print(unlabeled_data[decision,])
  return(decision)
}

m_derivat_function <- function(data, mu_priori, sigma_priori, theta_mu) {
  m_derivat <- likelihood_function(data = data, beta1 = theta_mu) * priori_function(theta_mu, mu_priori, sigma_priori)
  return(m_derivat)
}

m_mu_function <- function(data, mu_priori, sigma_priori) {
  g <- function(x) {
    return(m_derivat_function(data = data, mu_priori = mu_priori, sigma_priori = sigma_priori, theta_mu = x))
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
    # print(paste("Unten: ", f(seq[i]) , " Obern: " , f(seq[i+1])))
    if(f(seq[i]) * f(seq[i+1]) < 0 ) {
      inter <- c(seq[i], seq[i+1])
      roots <- c(roots, uniroot(f, inter)$root)
    }
  }
  return(roots)
}

AlphaCut_mu_function <- function(alpha, mu_priori_Lower, mu_priori_upper, data, sigma_priori) {
  m_mu_log_neg <- function(x) {
    return(-log(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori)))
  }
  max_par <- optim(par = 0, fn = m_mu_log_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  max_m <- m_mu_function(data = data, mu_priori = max_par, sigma_priori = sigma_priori)

  f <- function(x) {
    return(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori) - alpha * max_m)
    print(m_mu_function(data = data, mu_priori = x, sigma_priori = sigma_priori))
    print(alpha * max_m)
  }
  
  intervall <- c(mu_priori_Lower, mu_priori_upper)
  step = 0.01
  
  root <- root_function(f, intervall, step)
  return(root)
}
