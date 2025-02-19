calculateML <- function(priori, log_likelihood, boundary) {
  prior <- createPrior(density = priori[[1]], sampler = priori[[2]], lower = boundary[[1]], upper = boundary[[2]])
  setUp <- createBayesianSetup(likelihood = log_likelihood, prior = prior)
  out <- runMCMC(bayesianSetup = setUp, sampler = "DEzs", settings = list(iterations = 10000))
  log_ML <- marginalLikelihood(out, start = 1000)  # Ignoriere die ersten 500 Iterationen
  ML <- exp(log_ML$ln.ML)
  return(ML)
}

priori <- list()
priori[[2]] <-   function(n = 1) { mvnfast::rmvn(n = n, mu = c(1,1,1), sigma = diag(c(1,1,1)))}

priori[[1]]   <- function(theta) { mvnfast::dmvn(X = theta, mu = c(1,1,1), sigma = diag(c(1,1,1)))}

log_likelihood <- function(theta) {log( mvnfast::dmvn(X = theta, mu = c(1,1,1), sigma = diag(c(1,1,1))))}

boundary <- list(-5*c(1,1,1), 5*c(1,1,1))

calculateML(priori, log_likelihood, boundary)


##############################################################################
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



log_likelihood_logistic <- function(X, response) { 
  # Logistische Funktion
  logistic_function <- function(X, theta) {
    return(1 / (1 + exp(-X %*% theta)))  # Richtige logistische Funktion
  }
  
  # Log-Likelihood-Funktion
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    log_likelihood <- sum(response * log(p) + (1 - response) * log(1 - p))  # Korrekte Log-Likelihood
    return(log_likelihood)
  }
  
  return(likelihood)
}
get_log_likelihood <- function(labeled_data, glm_formula, target) {
  
  variables <- all.vars(glm_formula) #All variables involved in the regression.
  
  pred_variables <- variables[variables != target] #All variables involved in the regression except for the target variable.
  
  data_matrix <- as.matrix(selected_column <- subset(labeled_data, select = pred_variables)) #Design matrix without intercept
  
  X <- cbind(1, data_matrix) #Design matrix (with intercept)
  
  response <- as.matrix(selected_column <- subset(labeled_data, select = target)) #Response vector
  
  
  return(log_likelihood_logistic(X,response ))
}

labeled_data = data_frame[1:200,]
formula = target_var ~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6
target = "target_var"

log_likelihood <- get_log_likelihood(labeled_data, formula, target)
logistic_model <- glm(formula = formula,  data = labeled_data, family = "binomial")
max <- coef(logistic_model)
vcov(logistic_model)

cov(labeled_data)

max
log_likelihood(max)
opt <- optim(c(rep(0,7)), log_likelihood, control = list(fnscale = -1), hessian = TRUE)


log_likelihood(rep(1,7))
log_likelihood(rep(10,7))

####################################################

set.seed(123)
n <- 100
X <- cbind(1, runif(n, -2, 2))  # Intercept-Spalte + Prädiktorwerte
beta_true <- c(-1, 2)  
p <- 1 / (1 + exp(-X %*% beta_true))  
response <- rbinom(n, 1, p)  

# Eigene Log-Likelihood-Funktion
log_likelihood_logistic <- function(X, response) { 
  logistic_function <- function(X, theta) {
    return(1 / (1 + exp(-X %*% theta)))  
  }
  
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    return(sum(response * log(p) + (1 - response) * log(1 - p)))  # Korrekte Log-Likelihood
  }
  
  return(likelihood)
}

# Log-Likelihood-Funktion erstellen
log_likelihood_func <- log_likelihood_logistic(X, response)

# Maximierung mit optim()
optim_result <- optim(c(0, 0), log_likelihood_func, control = list(fnscale = -1))
optim_result$par  # Optimale Parameterwerte mit optim()

# Vergleich mit glm()
glm_model <- glm(response ~ X[, 2], family = binomial)
coef(glm_model)  # Parameter aus glm()
