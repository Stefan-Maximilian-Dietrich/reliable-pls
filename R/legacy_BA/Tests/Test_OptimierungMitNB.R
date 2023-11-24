
library('nloptr')


gamma_maximin_alpaC_function <- function(data_matrix, response, mu_priori_lower, mu_priori_upper, sigma_priori) {
  expected_utility <- function(x) {
    result <- expected_utility_function(data_matrix = data_matrix, response = response, mu_priori = x, sigma_priori = sigma_priori)
    print(result)
    return(result)
  }
  post_alpha_cut <- function(x) {
    m <- function(y) {
      result <- m_mu_function(data_matrix = data_matrix, response = response, mu_priori = y, sigma_priori = sigma_priori) 
      return(result)
    }
    m_min <- function(z) {
      result <- -log(m(z))
      return(result)
    }
    max_par <- optim(par = c(-8,0.4), fn = m_min, method = "BFGS")$par  
    max_m <- m(max_par)
    result <- m(x) - alpha * max_m
    return(result)
  }
  
  result <- nloptr(x0=c(0,0), eval_f=eval_f0, lb = mu_priori_lower, ub = mu_priori_upper, eval_g_ineq = eval_g0, opts = list("algorithm"="NLOPT_LN_COBYLA"))
  return(result)
}




eval_f0 <- function(x) {
  result <- expected_utility_function(data_matrix = A[[1]][[2]][1], response = A[[1]][[2]][2], mu_priori = x, sigma_priori = sigma_priori)
  print(result)
  return(result)
}

eval_g0 <- function(x) {
  h <- function(y) {
    result <- m_mu_function(data_matrix = A[[1]][[2]][1], response = A[[1]][[2]][2], mu_priori = y, sigma_priori) 
    return(result)
  }
  i <- function(y){
    result <- -log(h(y))
    return(result)
  }
  max_par <- optim(par = c(-8,0.4), fn = i, method = "BFGS")$par  #Ähnich wie Newtonverfahren 
  max_m <- m_mu_function(data_matrix = A[[1]][[2]][1], response = A[[1]][[2]][2], mu_priori = max_par, sigma_priori) 
  
  result <- m_mu_function(data_matrix = A[[1]][[2]][1], response = A[[1]][[2]][2], mu_priori = x, sigma_priori) - 0.7 * max_m
  return(result)
}





# Solve using NLOPT_LN_COBYLA without gradient information
res1 <- nloptr( x0=c(0,0),
                eval_f=eval_f0,
                lb = c(-20, -4),
                ub = c(4,8),
                eval_g_ineq = eval_g0,
                opts = list("algorithm"="NLOPT_LN_COBYLA"),
                )
print( res1 )


