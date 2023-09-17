data <- mtcars[c(1,2,4,8)]
ratio <- 0.9 
N <- nrow(data)
key <- seq(N)
key_lablet <- sample(x = key, size = round(N*ratio))
labeld_data <- data[c(key_lablet),]
unlabeled_data <- data[-c(key_lablet),]

### Referenz 
model1 <- glm(formula = vs ~ mpg  , data = data, family = "binomial")
summary(model1)

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

### Test N Dimensional 
theta <- c(17.9, -0.22, -1.32, -0.04)
mu_priori <- c(15, 0, -1, 0)
mu_priori_lower <- c(15, -1, -2, -1)
mu_priori_upper <- c(20, 1, 0, 1)
sigma_priori <- rbind(beta0 = c(1,0,0,0),cbind(beta0 = c(0,0,0),cov(mtcars[c(1,2,4)])))

set.seed(1)
data <- mtcars[c(1,2,4,8)]
ratio <- 0.9 
N <- nrow(data)
key <- seq(N)
key_lablet <- sample(x = key, size = round(N*ratio))
labeld_data <- data[c(key_lablet),]
unlabeled_data <- data[-c(key_lablet),]
labeld_data_matrix <- as.matrix(labeld_data[-c(4)])
unlabeled_data_matrix <- as.matrix(unlabeled_data[-c(4)])
response <- as.matrix(labeld_data[c(4)])
pseudo_response <- as.matrix(unlabeled_data[c(4)])

likelihood_function(theta = theta, data = data, response = response) 
priori_function(theta, mu_priori, sigma_priori)
utility_approximation_function(data_matrix = data_matrix, response = response, theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori) 
expected_utility_function(data_matrix = data_matrix , response = response, mu_priori = mu_priori, sigma_priori = sigma_priori)
gamma_maximin_function(a = 3, labeld_data_matrix = labeld_data_matrix, unlabeled_data_matrix = unlabeled_data_matrix, response = response, pseudo_response = pseudo_response, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori) 
decision_function(labeld_data = labeld_data, unlabeled_data = unlabeled_data, target = "vs",  mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori) 
m_derivat_function(data_matrix = labeld_data_matrix, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori, theta = theta)
m_mu_function (data_matrix = labeld_data_matrix, response = response, mu_priori = mu_priori, sigma_priori = sigma_priori)

### Test N Dimesionen aber nur 2 fenutzt (beta0 und beta1)
mu_priori2 <- c(-8, 0.3)
sigma_priori2 <- rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))
set.seed(1)
data2 <- mtcars[c(1,4)]
ratio <- 0.9 
N <- nrow(data2)
key <- seq(N)
key_lablet2 <- sample(x = key, size = round(N*ratio))
labeld_data2 <- data2[c(key_lablet2),]
unlabeled_data2 <- data2[-c(key_lablet2),]
labeld_data_matrix2 <- as.matrix(labeld_data2[-c(2)])
unlabeled_data_matrix2 <- as.matrix(unlabeled_data2[-c(2)])
response2 <- as.matrix(labeld_data2[c(2)])
pseudo_response2 <- as.matrix(unlabeled_data2[c(2)])
m_derivat_function(data_matrix = labeld_data_matrix2, response = response, mu_priori = c(-8.8888, 0.45), sigma_priori = sigma_priori2, theta = c(-8.8888, 0.45))
m_mu_function(data_matrix = labeld_data_matrix2, response = response2, mu_priori = c(-6, 0.45), sigma_priori = sigma_priori2)

seqenze2 <- seq(from = -40, to = 0, by = 1)
seqenze3<- seq(from = 0 , to = 1, by = 0.1)
df <- matrix(ncol = 3)
n <- 0
for(i in seqenze2) {
  for(j in seqenze3) {
    print(n/400)
    n <- n + 1
    df <- rbind(df, c(i,j, m_mu_function2(data_matrix = labeld_data_matrix2, response = response2, mu_priori = c(i, j), sigma_priori = sigma_priori2)))
  }
}
ggplot(data = as.data.frame(df), aes(x = V1, y = V2, fill = V3)) +
  geom_tile()

##

seqenze2 <- seq(from = -20, to = -10, by = 0.1)
seqenze3<- seq(from = 0, to = 2, by = 0.05)
df <- matrix(ncol = 3)
for(i in seqenze2) {
  for(j in seqenze3) {
    df <- rbind(df, (c(i,j, m_derivat_function(data_matrix = labeld_data_matrix2, response = response, mu_priori = c(-8.8, 0.5), sigma_priori = sigma_priori2, theta = c(i, j)))
))
  }
}
ggplot(data = as.data.frame(df), aes(x = V1, y = V2, fill = V3)) +
  geom_tile()
sum(df[,3], na.rm = T)



########################################### Ptotokoll
response1 <- mtcars[c(8)]
data_matrix1 <- mtcars[c(1)]
DF <- as.data.frame(cbind(response1, data_matrix1))
model1 <- glm(formula = vs ~ mpg  , data = DF, family = "binomial")
summary(model1)
sigma_priori <- rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))
m_derivat_function(data_matrix = data_matrix1, response1, mu_priori = c(-8, 0.3) , sigma_priori =  rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)]))), theta = c(-8, 0.3)) # ähnilch gut 
m_derivat_function(data_matrix = data_matrix1, response1, mu_priori = c(-8.8331, 0.4304) , sigma_priori =  rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)]))), theta = c(-8.8331, 0.4304)) # sehr gut gut 
m_derivat_function(data_matrix = data_matrix1, response1, mu_priori = c(-20, 2) , sigma_priori =  rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)]))), theta = c(-8.8331, 0.4304)) # änderung der priori
m_derivat_function(data_matrix = data_matrix1, response1, mu_priori = c(-20, 2) , sigma_priori =  rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)]))), theta = c(-12, 1)) # annäherung von theta an priori

m_mu_function(data_matrix = data_matrix1, response = response1, mu_priori = c(-8.8331, 0.4304), sigma_priori = rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))) # Priori und Likelyhood sind sich einig 
m_mu_function(data_matrix = data_matrix1, response = response1, mu_priori = c(-8.8331, 0), sigma_priori = rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))) # Priori und Likelyhood sind sich einig 
m_mu_function(data_matrix = data_matrix1, response = response1, mu_priori = c(-6, 2), sigma_priori = rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))) # Priori und Likelyhood sind sich einig 
m_mu_function(data_matrix = data_matrix1, response = response1, mu_priori = c(-10, 0.4304), sigma_priori = rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))) # Priori und Likelyhood sind sich einig 
m_mu_function(data_matrix = data_matrix1, response = response1, mu_priori = c(-30, 2), sigma_priori = rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))) # Priori und Likelyhood sind sich einig 


Test_function2D(beta0_u = -14, beta0_o = -4, step_beta0 = 0.1, beta1_u = -8, beta1_o = 8, step_beta1 = 0.1, data_matrix = data_matrix1, response = response1, sigma_priori = sigma_priori) 
  
######
data <- mtcars[c(1,8)]
ratio_tt <- 0.5
N <- nrow(data)
key_N <- seq(N)
key_test <- sample(x = key_N, size = round(N*ratio_tt))
test_data <- data[c(key_test),]
train_data <- data[-c(key_test),]
ratio_lu <- 0.5
M <- nrow(train_data)
key_M <- seq(M)
key_lable <- sample(x = key_M, size = round(M*ratio_lu))

labled_data <- train_data[c(key_lable),]
unlabled_data <- train_data[-c(key_lable),]
test_data <- data[c(key_test),]

sigma_priori <- rbind(beta0 = c(1,0),cbind(beta0 = c(0),cov(mtcars[c(1)])))
glm_formula <- as.formula("vs ~ mpg")
target <- "vs"
mu_priori_lower <- c(-14, -1)
mu_priori_upper <- c(-8, 8)
A <- gamma_maximin(labeled_data = labled_data, unlabeled_data = unlabled_data, test_data = test_data, target = "vs", glm_formula = formula, mu_priori_Lower = c(-14, -1), mu_priori_upper = c(-8, 8), sigma_priori = sigma_priori)

utility_approximation_function(A[[1]][[4]][1], A[[1]][[4]][2], theta = c(-20,2), mu_priori = c(-10,1), sigma_priori = sigma_priori)

expected_utility_function(data_matrix = A[[1]][[1]][1], response = A[[1]][[1]][2], mu_priori = c(2,2), sigma_priori = sigma_priori)
expected_utility_function(data_matrix = A[[1]][[2]][1], response = A[[1]][[2]][2], mu_priori = c(0,2), sigma_priori = sigma_priori)
expected_utility_function(data_matrix = A[[1]][[3]][1], response = A[[1]][[3]][2], mu_priori = c(-10,1), sigma_priori = sigma_priori)
expected_utility_function(data_matrix = A[[1]][[4]][1], response = A[[1]][[4]][2], mu_priori = c(-10,1), sigma_priori = sigma_priori)

gamma_maximin_function(data_fram = A[[1]][[1]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)
gamma_maximin_function(data_fram = A[[1]][[2]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)
gamma_maximin_function(data_fram = A[[1]][[3]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)
gamma_maximin_function(data_fram = A[[1]][[4]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)
gamma_maximin_function(data_fram = A[[1]][[5]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)
gamma_maximin_function(data_fram = A[[1]][[6]], target = "vs", mu_priori_lower = c(-35, -2), mu_priori_upper= c(0, 2), sigma_priori = sigma_priori)



expected_utility_function(data_matrix, response, mu_priori, sigma_priori) 

seqenze2 <- seq(from = -35, to = 0 , by = 0.5)
seqenze3<- seq(from = -2, to = 2, by = 0.1)
df <- matrix(ncol = 3)
n <- 0
for(i in seqenze2) {
  n <- n + 1
  print(paste(n/ length(seqenze2), " %"))
  for(j in seqenze3) {
    df <- rbind(df, c(i,j, (expected_utility_function(data_matrix = A[[1]][[1]][1],  response = A[[1]][[1]][2], mu_priori = c(i,j),  sigma_priori = sigma_priori))))
  }
}
df1 <- df
ggplot(data = as.data.frame(df2), aes(x = V1, y = V2, fill = V3)) +
  geom_tile()
View(df2)
View(df1)

################################################################################
(gamma_maximin(labeled_data = labled_data, unlabeled_data = unlabled_data, test_data = test_data, target = "vs", glm_formula = formula, mu_priori_Lower = c(-14, -1), mu_priori_upper = c(-8, 8), sigma_priori = sigma_priori))
