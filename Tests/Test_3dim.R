Test_m <- function(beta0_u, beta0_o, step_beta0, beta1_u, beta1_o, step_beta1, beta2_u, beta2_o, step_beta2, data_matrix, response, sigma_priori) {
  seqenze1 <- seq(from = beta0_u, to = beta0_o, by = step_beta0)
  seqenze2 <- seq(from = beta1_u, to = beta1_o, by = step_beta1)
  seqenze3 <- seq(from = beta2_u, to = beta2_o, by = step_beta2)
  rounds <- length(seqenze1) * length(seqenze2) * length(seqenze3)
  df <- matrix(ncol = 4)
  n <- 0
  for(i in seqenze1) {
    for(j in seqenze2) {
      for(k in seqenze3) {
        n <- n + 1
        df <- rbind(df, c(i,j,k, m_mu_function(data_matrix = data_matrix, response = response, mu_priori = c(i, j, k), sigma_priori = sigma_priori)))
        print(paste(n/rounds*100, "%"))
      }
    }
  }

  df <- df[-c(1),]
  df <- as.data.frame(df)
  print(df)
  colnames(df) <- c("beta0_priori", "beta1_priori", "beta2_priori", "m")
  
  return(df)
}

Test_alpah_cut <- function(data, alpha) {
  max_m <- max(data[, 4])
  data[data$m < alpha * max_m, ]$m <- NA
  return(data)
}

Test_e_utility <- function(data_matrix, response, mu_prioris, sigma_priori) {
  a <- 1
  labeld_data <- data_matrix[-(1),]
  unlabeled_data <- data_matrix[(1),]
  
  df <- matrix(ncol = 4)
  for(priori_nr in seq(nrow(mu_prioris))) {
    print(priori_nr)
    df <- rbind(df, c(mu_prioris[priori_nr, 1], mu_prioris[priori_nr, 2], mu_prioris[priori_nr, 3], expected_utility_function(data_matrix =data_matrix, response =response, mu_priori = c(mu_prioris[priori_nr, 1], mu_prioris[priori_nr, 2], mu_prioris[priori_nr, 3]), sigma_priori = sigma_priori)))
  }
  df <- as.data.frame(df)
  print(df)
  names(df) <- c("beta0_priori", "beta1_priori","beta2_priori", "e_utility")
  
  return(df)
} 



m_priori <- Test_m(beta0_u = -15, beta0_o = 15, step_beta0 = 3, beta1_u = -15, beta1_o = 15, step_beta1 = 3,  beta2_u = -15, beta2_o = 15, step_beta2 = 3, data_matrix = data_matrix2, response = response2, sigma_priori = sigma_priori2) 
priori <- m_priori[, c(1,2,3)]
m_priori_alpha <- Test_alpah_cut(data = m_priori, alpha = 0.7)
priori_alpha <- m_priori_alpha[, c(1,2,3)]
e_utility_priori <- Test_e_utility(data_matrix = data_matrix2, response = response2, mu_prioris = priori, sigma_priori = sigma_priori2)
e_utility_priori_alpha <- inner_join(e_utility_priori, m_priori_alpha, by = c("beta0_priori", "beta1_priori", "beta2_priori"))
e_utility_priori_alpha[!is.na(e_utility_priori_alpha$m),]$m <- 1
e_utility_priori_alpha$e_u <-  e_utility_priori_alpha$e_utility * e_utility_priori_alpha$m
e_utility_priori_alpha <- e_utility_priori_alpha[, c(1,2,3,6)]

names(e_utility_priori_alpha) <- c("beta0_priori", "beta1_priori", "e_utility")
Grafik_e_utility(e_utility_priori)
Grafik_e_utility(e_utility_priori_alpha)

