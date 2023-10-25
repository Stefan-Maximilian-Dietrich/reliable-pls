set.seed(2037420)


formula = target ~  Diagonal + Bottom + Length

data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'
data_frame$target <- as.numeric(data_frame$target == "genuine")

modell <- glm(formula = target ~  Diagonal + Bottom + Length, data = data_frame)

mu_priori_lower <- c(-45, -2, -2, -2)
mu_priori_lower <- c(-20, 2, 2, 2)
sigma_priori <-  rbind(beta0 = c(7,0,0,0),cbind(beta0 = c(0,0,0), cov(data_frame[c(7,5,2)]) ))
alpha = 0.8

data_frame_40 = data_frame[sample(nrow(data_frame), 40),]
test_40 <- anti_join(data_frame, data_frame_40)
unlabeld_40 <- data_frame_40[sample(nrow(data_frame_40), round(40*0.8)),]
labled_40 <- anti_join(data_frame_40, unlabeld_40)



likelihood_function <- function(theta, data_matrix, response) {
  #print("likelihood_function")
  
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
  #print("priori_function")
  
  priori <- mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori) #hier wirs nur die wahrscheilichekit des Mittelwerts betrachtet nicht die der sTREUUNG
  return(priori)
}

utility_approximation_function <- function(data_matrix, response, theta, mu_priori, sigma_priori) {
  #print("utility_approximation_function")
  
  fischer_info <-  sqrt((det(as.matrix(var(data_matrix)))))
  likelihood <- likelihood_function(theta = theta, data_matrix = data_matrix,  response = response)
  para_obs <- 2*pi/nrow(data_matrix)
  priori <- priori_function(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  
  return(para_obs * likelihood * priori / fischer_info)
}

expected_utility_function <- function(data_matrix, response, mu_priori, sigma_priori) {
  #print("expected_utility_function")
  
  
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
  
  print(paste("mu_priori_b0:", format(round(mu_priori[1], 4), nsmall = 5),"mu_priori_b1:", format(round(mu_priori[2], 4), nsmall = 5), "mu_priori_b2:", format(round(mu_priori[3], 4), nsmall = 5), "mu_priori_b3:", format(round(mu_priori[4], 4), nsmall = 5),"  e_utility:", result))
  #print(paste("mu_priori_b0:", format(round(mu_priori[1], 5), nsmall = 5),"mu_priori_b1:", format(round(mu_priori[2], 5), nsmall = 5), "  e_utility:", result))
  
  
  return(result)
} 

m_derivat_function <- function(data_matrix, response, mu_priori, sigma_priori, theta) {
  # print("m_derivat_function")
  
  m_derivat <- likelihood_function(theta = theta, data_matrix = data_matrix, response = response) * priori_function(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  return(m_derivat)
}

m_mu_function <- function(data_matrix, response, mu_priori, sigma_priori) {
  #print("m_mu_function")
  
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

m_alpha_function <- function(data_matrix , response , mu_priori , sigma_priori, alpha, m_max) {
  #print("m_alpha_function")
  
  
  result <-  m_mu_function(data_matrix = data_matrix, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori) - m_max * alpha
  
  return(result)
}

m_max_alpha_function <- function(data_matrix , response , sigma_priori, mu_priori_lower, mu_priori_upper) {
  #print("m_alpha_function")
  
  fn <- function(x) {
    result <- - m_mu_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori) 
    return(result)
  }
  
  x0 <- (mu_priori_lower + mu_priori_upper)/2
  m_max <- - nloptr(x0 = x0, eval_f = fn, lb = mu_priori_lower, ub = mu_priori_upper, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.001))$objective
  
  return(m_max)
}

gamma_maximin_alpaC_function <- function(data_matrix, response, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  #print("gamma_maximin_alpaC_function")
  
  m_max <- m_max_alpha_function(data_matrix = data_matrix , response = response , sigma_priori = sigma_priori, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper)
  print(m_max)
  
  
  expected_utility <- function(x) {
    result <- expected_utility_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(result)
  }
  
  m_alpha <- function(y) {
    result <- - m_alpha_function(data_matrix = data_matrix , response = response, mu_priori = y, sigma_priori = sigma_priori, alpha = alpha, m_max = m_max)
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
  #print("gamma_maximin_alpaC_addapter")
  variables <- all.vars(glm_formula)
  pred_variables <- variables[variables != target]
  data_matrix <- as.matrix(selected_column <- subset(data, select = pred_variables))
  response <- as.matrix(selected_column <- subset(data, select = target))
  
  result <- gamma_maximin_alpaC_function(data_matrix = data_matrix, response = response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)$objective
  return(result)
}



profvis({
  alpha_cut(labeled_data = labled_40[c(1,7,5,2)], unlabeled_data = unlabeld_40[c(7,5,2)], test_data = test_40[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha) 
})

