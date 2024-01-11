likelihood_function <- function(X, theta, response) {
  Y <- response
  
  logistic_function <- function(X, theta) {
    odds <- exp(X %*% theta)
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    #print(Y * p + (1 - Y) * (1 - p))
    likelihood <- prod(Y * p + (1 - Y) * (1 - p))
    return(likelihood)
  }
  
  result <- likelihood(theta = theta)
  return(result)
}

expected_utility_function <- function(theta, X, fischer_info, response, mu_priori, sigma_priori) {
  likelihood <- likelihood_function(X = X, theta = theta, response = response)
  para_obs <- 2*pi/(nrow(X) - 1)
  priori <-  mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori)
  if(is.na(priori)) {
    priori <- 0
  }
  print(priori)
  print(para_obs)
  print(likelihood)
  print(paste("PPP=", para_obs * likelihood * priori / fischer_info))
  return(para_obs * likelihood * priori / fischer_info)
} 

m_derivat_function <- function(X,  response, mu_priori, sigma_priori, theta) {
  m_derivat <- likelihood_function(X = X, theta = theta,  response = response) * mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori) 
  return(m_derivat)
}

m_function <- function(X,  response, mu_priori, sigma_priori) {
  g <- function(x) {
    return(m_derivat_function(X = X, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori, theta = x))
  }
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  
  start <- rep(0, times = ncol(X))
  theta_max <- optim(par = start, fn = h_neg, method = "BFGS")$par  #Ähnich wie Newtonverfahren
  hII_theta_max <- hessian(h, theta_max)
  
  m_approx <- exp( h(theta_max) ) * sqrt( (2*pi)^2 / abs(det(hII_theta_max))) 
  print(paste("m=", m_approx))
  return(m_approx)
} 

m_max_alpha_function <- function(X,  response , sigma_priori, mu_priori_lower, mu_priori_upper) {
  fn <- function(x) {
    result <- - m_function(X = X, response = response, mu_priori = x, sigma_priori = sigma_priori) 
    return(result)
  }
  
  start <- (mu_priori_lower + mu_priori_upper)/2
  m_max <- - nloptr(x0 = start, eval_f = fn, lb = mu_priori_lower, ub = mu_priori_upper, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.001))$objective
  
  return(m_max)
}

m_alpha_function <- function(X,  response , mu_priori , sigma_priori, alpha, m_max) {
  result <-  m_function(X = X,  response = response, mu_priori = mu_priori, sigma_priori = sigma_priori) - m_max * alpha
  return(result)
}

gamma_maximin_alpaC_function <- function(theta, X, fischer_info, response, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  m_max <- m_max_alpha_function(X = X, response = response , sigma_priori = sigma_priori, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper)
  
  expected_utility <- function(x) {
    result <- expected_utility_function(theta=theta, X = X, fischer_info = fischer_info, response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(result)
  }
  
  m_alpha <- function(y) {
    result <- - m_alpha_function(X = X, response = response, mu_priori = y, sigma_priori = sigma_priori, alpha = alpha, m_max = m_max)
    return(result)
  }
  
  x0 <- (mu_priori_upper + mu_priori_lower)/2
  print("Approximation der nidrigsten Expected Utility unter Nebenbedingung")
  
  result <- tryCatch({
    nloptr(x0=x0, eval_f = expected_utility, lb = mu_priori_lower, ub = mu_priori_upper, eval_g_ineq = m_alpha, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.001))  }, 
    error = function(e) {
      0  
    })
  return(result)
}

gamma_maximin_alpaC_addapter <- function(data, glm_formula, target, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  #browser()
  print("gamma_maximin_alpaC_addapter")
  variables <- all.vars(glm_formula)
  pred_variables <- variables[variables != target]
  data_matrix <- as.matrix(selected_column <- subset(data, select = pred_variables))
  X <- cbind(1, data_matrix)
  response <- as.matrix(selected_column <- subset(data, select = target))
  
  logistic_model <-  glm(data = data, formula = glm_formula, family = "binomial")
  not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))] ## 0 Falls NA's, ist ein Vektor der Covarianzmatix
  v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas))) ## ist wieder Matrix ohne NAs
  fisher_info <- v_cov_matrix %>% solve() ## invertierte Covarianzmatix
  
  theta <- coef(glm(data = data, formula = glm_formula, family = "binomial"))
  theta[is.na(theta)] <- 0
  print("Theta:")

  print(theta)
  ############
  
  result <- gamma_maximin_alpaC_function(theta = theta, X = X, fischer_info = fischer_info, response = response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)$objective
  return(result)
}
