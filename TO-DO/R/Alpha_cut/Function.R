#Likelihood function of logistic regression depending on theta, the design matrix, and the response vector
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

ppp_integral_derivat <- function(X,  response, theta, mu_priori, sigma_priori) {
  nrow <- nrow(X)
  likelihood_train <- likelihood_function(X = X[-nrow,], theta = theta, response = response[-nrow]) #Likelihood at the point theta depending on the design matrix and the response vector
  likelihood_new <- likelihood_function(X = X, theta = theta, response = response)
  priori <-  mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori) #Density of the normally distributed prior at the point theta, depending on mu and sigma
  return( likelihood_new * likelihood_train * priori)
}

ppp_integral <- function(X,  response, mu_priori, sigma_priori) {
  nrow <- nrow(X)
  g <- function(x) {
    return(ppp_integral_derivat(X = X,  response = response, theta = x, mu_priori = mu_priori, sigma_priori = sigma_priori))
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
  
  m_mu <- exp( h(x0) ) * sqrt( (2*pi)^2 / abs(det(hII_x0))) #Laplace Approximation
  return(m_mu)
} 

#Derivative of m consisting of likelihood and prior 
m_derivat_function <- function(X,  response, mu_priori, sigma_priori, theta) {
  nrow <- nrow(X)
  m_derivat <- likelihood_function(X = X[-nrow,], theta = theta, response = response[-nrow]) * mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori) 
  return(m_derivat)
}

#Calculation of m depending on the design matrix, the response vector, and a prior defined by its mu and sigma.
m_mu_function <- function(X,  response, mu_priori, sigma_priori) {
  g <- function(x) {
    return(m_derivat_function(X = X, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori, theta = x))
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
  
  m_mu <- exp( h(x0) ) * sqrt( (2*pi)^2 / abs(det(hII_x0))) #Laplace Approximation
  return(m_mu)
} 

#PPP function depending on the design matrix, the response vector, and the prior defined by mu and sigma.
pseudo_posterior_predictive_function <- function(X, response, mu_priori, sigma_priori) {
  m <- m_mu_function(X =X ,  response = response, mu_priori = mu_priori, sigma_priori = sigma_priori)
  int <- ppp_integral(X =X ,  response = response, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return((1/m)*int)
} 

#The function represents the constraint of the optimization; if it becomes less than 0, the respective prior is disqualified.
m_alpha_function <- function(X,  response , mu_priori , sigma_priori, alpha, m_max) {
  result <-  m_mu_function(X = X,  response = response, mu_priori = mu_priori, sigma_priori = sigma_priori) - m_max * alpha
  return(result)
}

#The function calculates the maximum of M
m_max_alpha_function <- function(X,  response , sigma_priori, mu_priori_lower, mu_priori_upper) {
  fn <- function(x) {
    result <- - m_mu_function(X = X, response = response, mu_priori = x, sigma_priori = sigma_priori) #The negation of the function (for minimization) corresponds to maximization.
    return(result)
  }
  
  x0 <- (mu_priori_lower + mu_priori_upper)/2 #Starting point of the maximization 
  m_max <- - nloptr(x0 = x0, eval_f = fn, lb = mu_priori_lower, ub = mu_priori_upper, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.001))$objective #Maximization 
  return(m_max)
}

#This function seeks the minimum of the ppp under the constraint that m is not less than alpha times the maximum of m
gamma_maximin_alpaC_function <- function(X,  response, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  m_max <- m_max_alpha_function(X = X, response = response , sigma_priori = sigma_priori, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper) #Calculation of the maximum m
  
  ppp <- function(x) { #Function to be optimized
    result <- pseudo_posterior_predictive_function(X = X,  response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(result)
  }
  
  m_alpha <- function(y) { #Constraint of the optimization 
    result <- - m_alpha_function(X = X, response = response, mu_priori = y, sigma_priori = sigma_priori, alpha = alpha, m_max = m_max) 
    return(result)
  }
  x0 <- (mu_priori_upper + mu_priori_lower)/2 #Starting point of the optimization
  result <- tryCatch({
    nloptr(x0=x0, eval_f = ppp, lb = mu_priori_lower, ub = mu_priori_upper, eval_g_ineq = m_alpha, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.001))  }, #Optimization function
    error = function(e) { #Fail-safe in case the optimization fails
      0  
    })
  return(result)
}

#The function serves to enable input similar to the inputs in other methods.
gamma_maximin_alpaC_addapter <- function(data, glm_formula, target, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  variables <- all.vars(glm_formula) #All variables involved in the regression.
  pred_variables <- variables[variables != target] #All variables involved in the regression except for the target variable.
  data_matrix <- as.matrix(selected_column <- subset(data, select = pred_variables)) #Design matrix without intercept
  X <- cbind(1, data_matrix) #Design matrix (with intercept)
  response <- as.matrix(selected_column <- subset(data, select = target)) #Response vector
  m <- length(pred_variables) +1
  mu_priori_lower <- mu_priori_lower[1:m]
  mu_priori_upper <- mu_priori_upper[1:m]
  sigma_priori <- sigma_priori[1:m,1:m]
  result <- gamma_maximin_alpaC_function(X = X,  response = response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)$objective
  return(result)
}
