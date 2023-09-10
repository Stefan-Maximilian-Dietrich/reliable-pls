data <- mtcars[c(1,8)]
ratio <- 0.9 
N <- nrow(data)
key <- seq(N)
key_lablet <- sample(x = key, size = round(N*ratio))
labeld_data <- data[c(key_lablet),]
unlabeled_data <- data[-c(key_lablet),]

seqenze <- seq(from = -2, to = 2, by = 0.01)
df <- matrix(ncol = 2)
for(i in seqenze) {
  df <- rbind(df, c(i, try(i * (utility_function(a =2 , labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = i, mu_priori = 1, sigma_priori = 2)))))
}
ggplot(data = as.data.frame(df), aes(x = V1, y = V2)) +
  geom_point()



m_derivat_function(data = data, mu_priori = 1, sigma_priori = 2, theta_mu = 0.5)
m_mu_function(data = data, mu_priori = 1, sigma_priori = 2)
AlphaCut_mu_function(alpha = 0.8, mu_priori_Lower = -2, mu_priori_upper = 5, data = data, sigma_priori = 2)
utility_function(a =3 , labeld_data = labeld_data, unlabeled_data = unlabeled_data, theta_mu = 2, mu_priori = 1, sigma_priori = 2)
expected_utility_function(a = 3, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori = 1, sigma_priori = 2)
gamma_maximin_function(a = 1, labeld_data = labeld_data, unlabeled_data = unlabeled_data, mu_priori_lower =0.3, mu_priori_upper= 0.6, sigma_priori = 2)

