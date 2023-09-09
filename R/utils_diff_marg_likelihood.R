library(dplyr)
# old
get_marg_l <- function(logistic_model) {
  fisher_info <- vcov(logistic_model) %>% solve() 
  n_parameters <- length(logistic_model$coefficients)
  marg_l <- -2 * logLik(logistic_model) + n_parameters + log(det(fisher_info)) -
    n_parameters * log(2*pi) 
  marg_l
}
#################################################### Normal ####################
get_log_marg_l <- function(logistic_model) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
      n_parameters <- length(logistic_model$coefficients)
      n_obs <- logistic_model$data %>% nrow
      cov_matrix <- vcov(logistic_model)
      if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){ ## wieso nur NA's ?
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))] ## 0 Falls NA's, ist ein Vektor der Covarianzmatix
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas))) ## ist wieder Matrix ohne NAs
      fisher_info <- v_cov_matrix %>% solve() ## invertierte Covarianzmatix
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) - 0.25 * log(det(fisher_info)) #von 3.5 bi Grundlagenpaier Formel 5 
      print(log_marg_l)
      }else{
        cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
        log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) 
      }
      log_marg_l
    })
}

################################### mit Priori #################################

install.packages("mvtnorm")
library(mvtnorm)


priori <- function(Modell) {
  coefficients <- logistic_model$coefficients
  number_coefficients <- length(logistic_model$coefficients)
  mean <- rep(0, number_coefficients)
  sigma <- einheitsmatrix(number_coefficients)
  result <- dmvnorm(coefficients, mean = mean, sigma = sigma)
  return(result)
}

einheitsmatrix <- function(n) {
  matrix <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    matrix[i, i] <- 1
  }
  
  return(matrix)
}

get_log_marg_l_priori <- function(logistic_model) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
    n_parameters <- length(logistic_model$coefficients)
    n_obs <- logistic_model$data %>% nrow
    cov_matrix <- vcov(logistic_model)
    if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){ ## wieso nur NA's ?
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))] ## 0 Falls NA's, ist ein Vektor der Covarianzmatix
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas))) ## ist wieder Matrix ohne NAs
      fisher_info <- v_cov_matrix %>% solve() ## invertierte Covarianzmatix
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) - 0.25 * log(det(fisher_info)) + log(priori(logistic_model))#von 3.5 bi Grundlagenpaier Formel 5 
    }else{
      cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) 
    }
    log_marg_l
  })
}

################################### ALLE Priori ################################
get_log_marg_l_alle_priori <- function(logistic_model, Priori_list) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
    n_parameters <- length(logistic_model$coefficients)
    n_obs <- logistic_model$data %>% nrow
    cov_matrix <- vcov(logistic_model)
    if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){ ## wieso nur NA's ?
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))] ## 0 Falls NA's, ist ein Vektor der Covarianzmatix
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas))) ## ist wieder Matrix ohne NAs
      fisher_info <- v_cov_matrix %>% solve() ## invertierte Covarianzmatix
      
      RESULT_log_marg_l <- c()
      for(i in seq(length(Priori_list))) {
        log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) - 0.25 * log(det(fisher_info)) + log(priori(logistic_model, Priori_list[[i]]))#von 3.5 bi Grundlagenpaier Formel 5
        RESULT_log_marg_l <- c(RESULT_log_marg_l, log_marg_l)
        
      }
      min_log_marg_l <- min(RESULT_log_marg_l)
      print(min_log_marg_l)
      min_log_marg_l
    }else{
      cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) 
    }
    log_marg_l
  })
}

