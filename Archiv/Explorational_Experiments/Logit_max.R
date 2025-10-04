set.seed(2138720)


n = 1000

feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = -2)
feature_3 <- rnorm(n, mean = 1.8)
feature_4 <- rnorm(n, mean = -1)
feature_5 <- rnorm(n, mean = 1.5, sd = 4)
feature_6 <- rnorm(n, mean = -1, sd = 8)

lin_comb <- 2.4- 7.9*feature_1 + 0.5*feature_2

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)

data_frame_test <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2,
                         feature_3 = feature_3, feature_4 = feature_4, feature_5 = feature_5, feature_6 = feature_6) #, feature_7 = feature_7, feature_8 = feature_8,


#########################
predict_from_formula <- function(formula,target, coefficients, new_data) {

  # Design-Matrix aus den Features der neuen Daten erstellen
  X_matrix <- model.matrix(formula, new_data)
  
  # Lineare Kombination berechnen (X * beta)
  linear_pred <- X_matrix %*% coefficients
  
  # Logistische Transformation (Sigmoid-Funktion)
  probability <- 1 / (1 + exp(-linear_pred)) 
  prediction <- as.numeric(probability > 0.5)
  sum <- sum(prediction == new_data$target_var)
  ratio <- sum/ nrow(new_data)
  return(ratio)  # Gibt Wahrscheinlichkeiten zurück
}


# simulate data
n = 100

feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = -2)
feature_3 <- rnorm(n, mean = 1.8)
feature_4 <- rnorm(n, mean = -1)
feature_5 <- rnorm(n, mean = 1.5, sd = 4)
feature_6 <- rnorm(n, mean = -1, sd = 8)

lin_comb <- 2.4- 7.9*feature_1 + 0.5*feature_2

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)

data_frame <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2,
                         feature_3 = feature_3, feature_4 = feature_4, feature_5 = feature_5, feature_6 = feature_6) #, feature_7 = feature_7, feature_8 = feature_8,



log_likelihood_logistic <- function(X, response) { 
  # Logistische Funktion
  logistic_function <- function(X, theta) {
    return(1 / (1 + exp(-X %*% theta)))  # Richtige logistische Funktion
  }
  
  # Log-Likelihood-Funktion
  likelihood <- function(theta) {
    p <- logistic_function(X, theta) + 10^(-300)

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


true <- c(2.4, -7.9, 0.5, 0,0,0,0)

glm_result <- NULL

for(i in 1:n) {
  labeled_data = data_frame[1:i,]
  logistic_model <- glm(formula = formula,  data = labeled_data, family = "binomial")
  new <- predict_from_formula(formula, target_var, coefficients = coef(logistic_model), new_data = data_frame_test)
  glm_result <- c(glm_result, new)
  #glm_result <- rbind(glm_result, coef(logistic_model))
}

otim_result <- NULL
for(i in 1:n) {
  labeled_data = data_frame[1:i,]
  log_likelihood <- get_log_likelihood(labeled_data, formula, target)
  paras <- optim(c(rep(0,7)), log_likelihood, control = list(fnscale = -1))$par
  new <- predict_from_formula(formula, target_var, coefficients = paras, new_data = data_frame_test)
  otim_result <- c(otim_result, new)
  #otim_result <- rbind(otim_result, paras)
}

df <- data_frame(glm_result, otim_result, sum(na.rm = TRUE , as.numeric(otim_result - glm_result)))[1:100,]
View(df)






################################################################################


labeled_data = data_frame[1:200,]
formula = target_var ~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6
target = "target_var"

log_likelihood <- get_log_likelihood(labeled_data, formula, target)
logistic_model <- glm(formula = formula,  data = labeled_data, family = "binomial")
max <- coef(logistic_model)

max
log_likelihood(max)
optim(c(rep(0,7)), log_likelihood, control = list(fnscale = -1))


log_likelihood(rep(1,7))
log_likelihood(rep(10,7))



#to do
log likely ersetzen bei ml und argmax ersätzen später
