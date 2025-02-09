
library(BayesianTools)


################################################################################
normal_radnom_spaced <- function(n, a,b){
  sigma_priori <- diag(length(a))
  mu <- list()
  
  for(i in 1:n){
    mu_priori <- c()
    for(j in 1:length(a)) {
      mu_priori <- c(mu_priori, runif(1, min = a[j], max = b[j]))
    }
    mu[[i]] <- mu_priori
  }
  
  
  FUN_density <- function(mu) {
    return( function(theta) { mvnfast::dmvn(X = theta, mu = mu, sigma = sigma_priori)})
  }
  
  FUN_sampler <- function(mu) {
    return( function(n = 1) { mvnfast::rmvn(n = n, mu = mu, sigma = sigma_priori)})
  }
  
  
  density <- lapply(mu, FUN_density)
  sampler <- lapply(mu, FUN_sampler)
  
  combined_list <- Map(list, density, sampler)
  
  
  return(combined_list)
}
prioris = normal_radnom_spaced(1, c(-2,-2,-2,-2,-2,-2,-2), c(-2,-2,-2,-2,-2,-2,-2))
prioris1 = normal_radnom_spaced(1, c(2,2,2,2,2,2,2), c(2,2,2,2,2,2,2))
prioris3 = normal_radnom_spaced(1, c(100,100,100,100,100,100,100), c(100,100,100,100,100,100,100))

log_likelihood_logistic <- function(X, response) {
  
  Y <- response1
  
  logistic_function <- function(X, theta) {
    
    probability <- 1/ (1 + 1/exp(X %*% theta)) #Odds
    return(probability)
  }
  
  likelihood <- function(theta) {
    
    
    p <- logistic_function(X, theta)
    likelihood <- log(prod(Y * p + (1 - Y) * (1 - p)))
    return(likelihood)
  }
  
  return(likelihood)
}

labeled_data1 = as.matrix(cbind(1,data_frame[1:20,]))
labeled_data2 = as.matrix(cbind(1,data_frame[2:21,]))

response1 <- labeled_data1[,c(1)]
X1<- labeled_data1[,-c(1)]
log_likelihood1 <- log_likelihood_logistic(X1, response1)

likelihood <- likelihood_logistic(X1, response1)

response2 <- labeled_data2[,c(1)]
X2 <- labeled_data2[,-c(1)]
log_likelihood2 <- log_likelihood_logistic(X2, response2)

boundary <- list(1000*c(-2,-2,-2,-2,-2,-2,-2), 1000*c(2,2,2,2,2,2,2))

calculateML <- function(priori, log_likelihood, boundary) {
  prior <- createPrior(density = priori[[1]], sampler = priori[[2]], lower = boundary[[1]], upper = boundary[[2]])
  setUp <- createBayesianSetup(likelihood = log_likelihood, prior = prior)
  print(setUp)
  out <- runMCMC(bayesianSetup = setUp, sampler = "DEzs", settings = list(iterations = 20000))
  log_ML <- marginalLikelihood(out, start = 2000)  # Ignoriere die ersten 500 Iterationen
  ML <- c(log_ML$ln.ML)
  return(ML)
}

#################################################################################

A_P1 <- calculateML(priori = prioris[[1]], log_likelihood = log_likelihood1, boundary = boundary)
A_P2 <- calculateML(priori = prioris1[[1]], log_likelihood = log_likelihood1, boundary = boundary)
A_P3 <- calculateML(priori = prioris3[[1]], log_likelihood = log_likelihood1, boundary = boundary)

B_P1 <- calculateML(priori = prioris[[1]], log_likelihood = log_likelihood1, boundary = boundary)
B_P2 <- calculateML(priori = prioris1[[1]], log_likelihood = log_likelihood1, boundary = boundary)
B_P3 <- calculateML(priori = prioris3[[1]], log_likelihood = log_likelihood1, boundary = boundary)

C_P1 <- calculateML(priori = prioris[[1]], log_likelihood = log_likelihood1, boundary = boundary)
C_P2 <- calculateML(priori = prioris1[[1]], log_likelihood = log_likelihood1, boundary = boundary)
C_P3 <- calculateML(priori = prioris3[[1]], log_likelihood = log_likelihood1, boundary = boundary)


data.frame(c(A_P1, A_P2, A_P3), c(B_P1, B_P2, B_P3), c(C_P1, C_P2, C_P3))

################################################################################
controll <- function(n, priori, likelihood) {
  x1 <- 0
  x2 <- 0
  x3 <- 0
  a <- c(20,20,20,20,20,20,20)
  b <- -1 * a
  

  for(i in 1:n){
    
    x <- c()
    for(j in 1:length(a)) {
      x <- c(x, runif(1, min = b[j], max = a[j]))
    }

    if(priori(x) > 0) { x1 = x1 + 1}
    if(likelihood(x) > -Inf) {x2 = x2 + 1}
    if(likelihood(x) > -Inf && priori(x) > 0) {x3 = x3 + 1}
  }
  return(c(x1, x2, x3))
}

############################################################################



likelihood <- log_likelihood_logistic(X1, response1)
likelihood(rep(2,7))

controll(n = 20000, priori = prioris[[1]][[1]], likelihood = likelihood) 
