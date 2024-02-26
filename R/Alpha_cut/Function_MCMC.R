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

ppp_integral <- function(X,  response, mu_priori, sigma_priori) {
  nrow <- nrow(X)
  pseudo_log_likelihood <-  function(theta) {
    return(log(likelihood_function(X=X, theta =theta, response = response) * likelihood_function(X=X[-nrow,], theta =theta, response = response[-nrow])))
  }
  density = function(theta){
    d1 =  mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori, log = TRUE) 
    return(d1)
  }
  sampler = function(n=1){
    d1 = rmnorm(n, mean = mu_priori, varcov = sigma_priori)
    return(d1)
  }
  prior <- createPrior(density = density, sampler = sampler, lower = c(-20, -10, -10, -10, -10, -10, -10), upper = c(10,20,10,10,10,10,10), best = NULL)
  setUp <- createBayesianSetup(log_likelihood, prior =  prior)
  out <- runMCMC(bayesianSetup = setUp)
  log_ML = marginalLikelihood(out, start = 1000)
  ML <- exp(log_ML$ln.ML)
  
  return(ML)
} 

#Derivative of m consisting of likelihood and prior 
m_derivat_function <- function(X,  response, mu_priori, sigma_priori, theta) {
  nrow <- nrow(X)
  m_derivat <- likelihood_function(X = X[-nrow,], theta = theta, response = response[-nrow]) * mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori) 
  return(m_derivat)
}

#Calculation of m depending on the design matrix, the response vector, and a prior defined by its mu and sigma.
m_mu_function <- function(X,  response, mu_priori, sigma_priori) {
  nrow <- nrow(X)
  log_likelihood <-  function(theta) {
    return(log(likelihood_function(X=X[-nrow,], theta =theta, response = response[-nrow])))
  }
  density = function(theta){
    d1 =  mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori, log = TRUE) 
    return(d1)
  }
  sampler = function(n=1){
    d1 = rmnorm(n, mean = mu_priori, varcov = sigma_priori)
    return(d1)
  }
  prior <- createPrior(density = density, sampler = sampler, lower = c(-20, -10, -10, -10, -10, -10, -10), upper = c(10,20,10,10,10,10,10), best = NULL)
  setUp <- createBayesianSetup(log_likelihood, prior =  prior)
  out <- runMCMC(bayesianSetup = setUp)
  log_ML = marginalLikelihood(out, start = 1000)
  ML <- exp(log_ML$ln.ML)
  
  return(ML)
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
  
  result <- gamma_maximin_alpaC_function(X = X,  response = response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)$objective
  return(result)
}
