gamma_maximin_alpaC_function_test <- function(data_matrix, response, mu_priori_lower, mu_priori_upper, sigma_priori, alpha) {
  print("im Test")
  expected_utility <- function(x) {
    result <- expected_utility_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori)
    return(result)
  }
  
  m_alpha <- function(y) {
    result <- - m_alpha_function(data_matrix = data_matrix , response = response, mu_priori = y, sigma_priori = sigma_priori, alpha = alpha)
    return(result)
  }
  

  x0 <- (mu_priori_upper + mu_priori_lower)/2
  print("Approximation der nidrigsten Expected Utility unter Nebenbedingung")
  result <- nloptr(x0=x0, eval_f = expected_utility, lb = mu_priori_lower, ub = mu_priori_upper, eval_g_ineq = m_alpha, opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel" = 0.1))
  
  return(result)
}



#Tests
#100/50
start <- Sys.time()
Test100_4 <- alpha_cut(labeled_data = data100, unlabeled_data = df[101, c(2,3,4)], test_data = data1000_t, target =  "y", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
End <- Sys.time()
End - start
