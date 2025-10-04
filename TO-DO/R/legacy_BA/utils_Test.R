# creedal Set 
# 
install.packages("mvtnorm")
library(mvtnorm)


priori <- function(Modell, Priori) {
  coefficients <- logistic_model$coefficients
  mean <- Priori[[1]]
  sigma <- Priori[[2]]
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

Priori_Mean_Generator <- function(Mittelpunkt = c(0,0,0,0)) { 
  Mean <- c(rnorm(1, Mittelpunkt[1]), rnorm(1, Mittelpunkt[2]), rnorm(1, Mittelpunkt[3]), rnorm(1, Mittelpunkt[4]))
  return(Mean)
}

Priori_list <- list()
for(i in seq(10)) {
  Priori_list[[i]] <- list(Priori_Mean_Generator(), einheitsmatrix(4))
}

###############################################################################

likelihood_function <- function(data, mu, sigma) {
  likelihood <- prod(dnorm(data, mean = mu, sd = sigma))
  return(likelihood)
}

# Beispiel: Beobachtete Daten
data <- c(1.5, 2.0, 2.5, 1.8, 2.2)

# Modellparameter schätzen (z.B., aus Schätzungen oder Maximum-Likelihood-Schätzungen)
mu_hat <- mean(data)
sigma_hat <- sd(data)

# Likelihood für die gegebenen Daten und Modellparameter berechnen
likelihood <- likelihood_function(data, mu = mu_hat, sigma = sigma_hat)

# Ausgabe der Likelihood
print(likelihood)



######### Priori Spezifikation ###########################################
mean <- c(-2,   -1,   0,    1,    2,    3,    4) 
sd <-   c(1.73, 1.73, 1.73, 1.73, 1.73, 1.73, 1.73)
Prioris <- cbind(mean, sd)

Priori_Samples <- list()
for(i in seq(nrow(Prioris))) {
  print(i)
  x <- rnorm(10000, mean = Prioris[i,1], sd = Prioris[i,2])
  Priori_Samples[[i]] <- 1/(1+exp(-x))
}

######### Prori ##########################################################
make_discrete_Priori <- function(Priori_Sample) {
  Wahrscheilichkeit <- matrix(ncol = 2, nrow = 0)
  colnames(Wahrscheilichkeit) <- c("p", "pp")
  Sequenz <- seq(from = 0, to = 1, by = 0.05)
  for(i in 2:length(Sequenz)) {
    p <- (Sequenz[i-1] + Sequenz[i])/2
    n = 0
    for(j in seq_along(Priori_Sample)) {
      n = n + ifelse(Priori_Sample[j] >= Sequenz[i-1] && Priori_Sample[j] < Sequenz[i], 1, 0)
    }
    N <- length(x)
    pp <- n/N
    Wahrscheilichkeit <- rbind(Wahrscheilichkeit, c(p = p, pp = pp))
  }
  return(Wahrscheilichkeit)
}


Creedal_Set <- list()
for(i in seq(length(Priori_Samples))) {
  hist(Priori_Samples[[i]])
  Creedal_Set[[i]] <- make_discrete_Priori(Priori_Samples[[i]])
}
###########################################################################

###### Utlity in abhängikeit der Priori####################################

###########################################################################

######## Alpha-Cut ########################################################
marginal_likelihood <- function(p)

do_AlphaCut <- function(Prioris, Alpha) {
  for(i in nrow(Prioris)) {
    max <- which.max(Wahrscheilichkeit[,2])
    max
  }
}
###########################################################################

m <- function()