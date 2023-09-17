Test_m <- function(beta0_u, beta0_o, step_beta0, beta1_u, beta1_o, step_beta1, data_matrix, response, sigma_priori) {
  seqenze2 <- seq(from = beta0_u, to = beta0_o, by = step_beta0)
  seqenze3<- seq(from = beta1_u, to = beta1_o, by = step_beta1)
  rounds <- length(seqenze2) * length(seqenze3)
  df <- matrix(ncol = 3)
  n <- 0
  for(i in seqenze2) {
    for(j in seqenze3) {
      n <- n + 1
      df <- rbind(df, c(i,j, m_mu_function(data_matrix = data_matrix, response = response, mu_priori = c(i, j), sigma_priori = sigma_priori)))
      print(paste(n/rounds*100, "%"))
    }
  }

  df <- df[-c(1),]
  df <- as.data.frame(df)
  colnames(df) <- c("beta0_priori", "beta1_priori", "m")
  
  return(df)
}

Test_alpah_cut <- function(data, alpha) {
  max_m <- max(data[, 3])
  data[data$m < alpha * max_m, ]$m <- NA
  return(data)
}

Test_e_utility <- function(data_matrix, response, mu_prioris, sigma_priori) {
  a <- 1
  labeld_data <- data_matrix[-(1),]
  unlabeled_data <- data_matrix[(1),]
  
  df <- matrix(ncol = 3)
  for(priori_nr in seq(nrow(mu_prioris))) {
    print(priori_nr)
  df <- rbind(df, c(mu_prioris[priori_nr, 1], mu_prioris[priori_nr, 2], expected_utility_function(data_matrix =data_matrix, response =response, mu_priori = c(mu_prioris[priori_nr, 1], mu_prioris[priori_nr, 2]), sigma_priori = sigma_priori)))
  }
  df <- as.data.frame(df)
  names(df) <- c("beta0_priori", "beta1_priori", "e_utility")

  return(df)
} 

Grafik_m <- function(df) {
  ggplot(data = df, aes(x = beta0_priori, y = beta1_priori, fill = m)) +
    geom_tile()
}

Grafik_e_utility <- function(df, limes) {
  ggplot(data = df, aes(x = beta0_priori, y = beta1_priori, fill = e_utility)) +
    geom_tile() +
    geom_point(data = subset(df, e_utility == min(e_utility, na.rm = TRUE)), aes(x = beta0_priori, y = beta1_priori), color = "red", size = 3)  # Tiefpunkt als Punkt markieren
}

m_priori <- Test_m(beta0_u = -14, beta0_o = -4, step_beta0 = 0.1, beta1_u = -8, beta1_o = 8, step_beta1 = 0.1, data_matrix = data_matrix1, response = response1, sigma_priori = sigma_priori) 
priori <- m_priori[, c(1,2)]
Grafik_m(m_priori)
m_priori_alpha <- Test_alpah_cut(data = m_priori, alpha = 0.7)
priori_alpha <- m_priori_alpha[, c(1,2)]
Grafik_m(m_priori_alpha)
e_utility_priori <- Test_e_utility(data_matrix = data_matrix1, response = response1, mu_prioris = priori, sigma_priori = sigma_priori)
e_utility_priori_alpha <- inner_join(e_utility_priori, m_priori_alpha, by = c("beta0_priori", "beta1_priori"))
e_utility_priori_alpha[!is.na(e_utility_priori_alpha$m),]$m <- 1
e_utility_priori_alpha$e_u <-  e_utility_priori_alpha$e_utility * e_utility_priori_alpha$m
e_utility_priori_alpha <- e_utility_priori_alpha[, c(1,2,5)]
names(e_utility_priori_alpha) <- c("beta0_priori", "beta1_priori", "e_utility")
Grafik_e_utility(e_utility_priori)
Grafik_e_utility(e_utility_priori_alpha)




#Normal
df <- Test_function2D(beta0_u = -14, beta0_o = -4, step_beta0 = 0.1, beta1_u = -8, beta1_o = 8, step_beta1 = 0.1, data_matrix = data_matrix1, response = response1, sigma_priori = sigma_priori) 
df_alpha <- Test_alpah_cut(data = df, alpha = 0.7)



#Doppelte Menge an datenpunkten
data_matrix3 <- rbind(data_matrix1,data_matrix1)
response3 <- rbind(response1,response1)
df2 <- Test_function2D(beta0_u = -14, beta0_o = -4, step_beta0 = 0.1, beta1_u = -8, beta1_o = 8, step_beta1 = 0.1, data_matrix = data_matrix3, response = response3, sigma_priori = sigma_priori) 

# 10 fache menge 
data_matrix10 <- rbind(data_matrix1,data_matrix1, data_matrix1,data_matrix1, data_matrix1,data_matrix1, data_matrix1,data_matrix1,data_matrix1,data_matrix1)
response10 <- rbind(response1,response1, response1,response1, response1,response1,response1,response1,response1,response1)
df10 <- Test_function2D(beta0_u = -14, beta0_o = -4, step_beta0 = 0.1, beta1_u = -8, beta1_o = 8, step_beta1 = 0.1, data_matrix = data_matrix10, response = response10, sigma_priori = sigma_priori) 
alpah_cut(data = df10, alpha = 0.7)

