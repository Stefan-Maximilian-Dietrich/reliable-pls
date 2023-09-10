library(ggplot2)
install.packages("rootSolve")
library("rootSolve")

install.packages("pracma")
library("pracma")


data <- matrix(data = c(1,1,
                        0,3,
                        2,3,
                        2,2,
                        2,22),
               ncol  = 1)

mu_theta <- seq(from = -5, to = 10, by = 0.01 ) #in dem Fall der Mitelwert 
sigma_theta <- rep(1, length(mu_theta))

A <- m_derivat_function(data = data, mu_priori = 0, sigma_priori = 1, theta_mu = 0, theta_sigma = 1)
B <- m_derivat_function(data = data, mu_priori = 0, sigma_priori = 1, theta_mu = 0, theta_sigma = 1)

f <- function(x) {
  result <- m_derivat_function(data = data, mu_priori = 0, sigma_priori = 1, theta_mu = x, theta_sigma = 1)
  return(result) 
}



f_log <- function(x) {
  return(-log((f(x))))
}


result <- optim(par = 0, fn = f_log, method = "BFGS") # Ähnlich wie Newtenschen verfahen 


Datapoitns <- matrix(ncol = 2)
for(k in seq_along(mu_theta)) {
  Datapoitns <- rbind(Datapoitns, c(mu_theta[k], f_log(mu_theta[k])))
}
View(Datapoitns)

DF <- as.data.frame(Datapoitns)

ggplot(DF, aes(x = V1, y = V2)) +
  geom_point()


m_mu_functio(data = data, mu_priori = 0, sigma_priori = 1, theta_sigma = 1)
Test_f <- function(x) {
  return((m_mu_functio(data = data, mu_priori = x, sigma_priori = 1, theta_sigma = 1) - 8.435553e-87 * 0.7))
}

Datapoitns <- matrix(ncol = 2)
for(k in seq_along(mu_theta)) {
  Datapoitns <- rbind(Datapoitns, c(mu_theta[k], Test_f(mu_theta[k])))
}

DF <- as.data.frame(Datapoitns)[-c(1),]
View(DF)
max(DF$V2)


ggplot(DF, aes(x = V1, y = V2)) +
  geom_point() 

root_function(Test_f, intervall = c(-3, 10), step = 0.1)
#################################################################################

Datapoitns <- matrix(ncol = 2)
for(k in seq_along(mu_theta)) {
  Datapoitns <- rbind(Datapoitns, c(mu_theta[k], m_mu_functio(data = data, mu_priori = mu_theta[k], sigma_priori = 1, theta_sigma = 1)))
}


DF <- as.data.frame(Datapoitns)[-c(1),]
View(DF)
max(DF$V2)


ggplot(DF, aes(x = V1, y = V2)) +
  geom_point() +
  geom_hline(yintercept= 0.6 * max(DF$V2))



AC <- AlphaCut_mu(alpha = 0.6, mu_priori_Lower = -5, mu_priori_upper = 10, data = data, sigma_priori = 1, theta_sigma = 1)
