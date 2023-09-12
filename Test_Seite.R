data <- mtcars[c(1,8)]
ratio <- 0.9 
N <- nrow(data)
key <- seq(N)
key_lablet <- sample(x = key, size = round(N*ratio))
labeld_data <- data[c(key_lablet),]
unlabeled_data <- data[-c(key_lablet),]

### Test 1 Dim 
seqenze <- seq(from = -2, to = 2, by = 0.01)
df <- matrix(ncol = 2)
for(i in seqenze) {
  df <- rbind(df, c(i, try(i * (utility_function(a =2 , labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = i, mu_priori = 1, sigma_priori = 2)))))
}
ggplot(data = as.data.frame(df), aes(x = V1, y = V2)) +
  geom_point()

### Test 2 Dim
seqenze2 <- seq(from = -30, to = 0, by = 0.02)
seqenze3<- seq(from = 0, to = 1.5, by = 0.001)
df <- matrix(ncol = 3)
for(i in seqenze2) {
  for(j in seqenze3) {
    df <- rbind(df, c(i,j, (likelihood_function(beta0 = i, beta1 = j, data = labeld_data) )))
  }
}
ggplot(data = as.data.frame(df), aes(x = V1, y = V2, fill = V3)) +
  geom_tile()



m_derivat_function(data = data, mu_priori = 1, sigma_priori = 2, theta_mu = 0.5)
m_mu_function(data = data, mu_priori = 1, sigma_priori = 2)
AlphaCut_mu_function(alpha = 0.8, mu_priori_Lower = -2, mu_priori_upper = 5, data = data, sigma_priori = 2)
likelihood_function(beta0 = 20, beta1 = 20, data = data) 
priori_function(theta = c(-8, 0.4), mu_priori = c(-8, 0.4), sigma_priori = matrix(data = c(2,-1,-1,1), nrow = 2)) 
utility_approximation_function(a = 2 , labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta = c(-8, 0.4), mu_priori= c(-8, 0.4), sigma_priori = matrix(data = c(2,-1,-1,1), nrow = 2))
expected_utility_function(a = 3, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori= c(-8, 0.4), matrix(data = c(2,-1,-1,1), nrow = 2))
gamma_maximin_function(a = 1, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori_lower = c(-10,0), mu_priori_upper= c(0, 1), sigma_priori = matrix(data = c(2,-1,-1,1), nrow = 2))
decision_function(labeld_datam = labeld_datam, unlabeled_data = unlabeled_data, mu_priori_lower = c(-10,0), mu_priori_upper = c(0, 1), matrix(data = c(2,-1,-1,1), nrow = 2)) 
