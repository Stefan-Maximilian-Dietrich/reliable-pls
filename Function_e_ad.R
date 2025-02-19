normal_radnom_spaced <- function(n, a,b){
  sigma_priori <- diag(length(a)) * 200
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

log_likelihood_logistic <- function(X, response) { 
  
  likelihood <- function(theta) {
    p <- 1 / (1 + exp(-X %*% theta))
    log_likelihood <- sum(response * log(p+ 10^(-280)) + (1 - response) * log(1 - p + 10^(-280)))  # Korrekte Log-Likelihood
    #print( log(p))
    #print(log(1 - p))
    #print(log_likelihood)
    
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

logistic_modell_function <- function(labeld_data, pseudo_data, formula) {
  data <- apply(pseudo_data,1, function(x) {rbind(labeld_data, x)})
  
  target <- all.vars(formula)[1]
  logistic_modells <- lapply(data, get_log_likelihood, glm_formula = formula, target = target)
  
  return(logistic_modells)
}

get_log_marg_l <- function(logistic_model, max) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
    n_parameters <- length(logistic_model$coefficients)
    n_obs <- logistic_model$data %>% nrow
    cov_matrix <- vcov(logistic_model)
    if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){
      
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))]
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas)))
      fisher_info <- v_cov_matrix %>% solve() 
      
      log_marg_l <- max + n_parameters/2 * log(2*pi/n_obs) - 0.25 * log(det(fisher_info))
    }else{
      cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
      log_marg_l <- max + n_parameters/2 * log(2*pi/n_obs) 
    }
    log_marg_l
  })
}

calculateML <- function(priori, log_likelihood, boundary) {
  prior <- createPrior(density = priori[[1]], sampler = priori[[2]], lower = boundary[[1]], upper = boundary[[2]])
  setUp <- createBayesianSetup(likelihood = log_likelihood, prior = prior)
  out <- runMCMC(bayesianSetup = setUp, sampler = "DEzs", settings = list(iterations = 15000))
  log_ML <- marginalLikelihood(out, start = 2000)  # Ignoriere die ersten 500 Iterationen
  ML <- exp(log_ML$ln.ML)
  return(ML)
}

marginal_likelihood <- function(prioris, log_likelihood, boundary) {
  Marginal_list <- c()
  for(i in 1:length(prioris)) {
    new <- calculateML(prioris[[i]], log_likelihood, boundary) 
    Marginal_list <- c(Marginal_list, new)
  }
  return(Marginal_list)
}

alpha_cut <- function(prioris, log_likelihood, alpha, boundary) {
  marginal <- marginal_likelihood(prioris, log_likelihood, boundary)
  marginal[is.na(marginal)] <- -Inf
  print(marginal)
  max <- max(marginal, na.rm = TRUE)
  decision <- marginal >= alpha * max
  updated_prioris <- prioris[decision]
  return(updated_prioris)
}

ppp <- function(dims, priori, log_likelihood) {
  n_var <- dims[1]
  n_obs <- dims[2]
  
  
  otim <-  optim(rep(0,n_var), log_likelihood, control = list(fnscale = -1), hessian = TRUE)
  max <- otim$value
  argmax <- otim$par
  hessian <- otim$hessian
  
  
  ######
  if(!is.na(max)) {
    max <- max
  } else {
    max <- 0
  }

  
  if(!is.na(priori[[1]](argmax))) {
    pri <- priori[[1]](argmax)
  } else{
    pri <- 0
  }
  
  if(log(det(-hessian) != -Inf )) {
    hess <- log(det(-hessian))
  } else{
    hess <- 0
  }
  
  print(paste("log: ", exp(max), " priori: ", pri, " hessian: ", exp(hess)))
  
  ########
  result <- exp(max)  * pri / exp(0.25 * hess)
  return(result)
}

decision_matrix <- function(dims, logistic_models, cut_prioris) {
  k <- length(logistic_models)
  l <- length(cut_prioris)
  matrix <- matrix(NA, nrow = k, ncol = l)
  for(i in 1:k) {
    for(j in 1:l) {
      matrix[i,j] <- ppp(dims, cut_prioris[[j]], logistic_models[[i]])
    }
  }
  print(matrix)
  return(matrix)
}

check_matrix_condition <- function(mat, ref_rowl) {
  # Bestimme die Anzahl der Reihen und Spalten
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)
  
  # Überprüfe für jede Spalte, ob die Bedingung erfüllt ist
  for (j in 1:n_rows) {
    # Gehe jede Zeile durch und vergleiche den Wert in der Referenzspalte mit dem Wert in der j-ten Spalte
    if (all(mat[ref_rowl, ] < mat[j, ])) {
      return(FALSE)  # Wenn in der j-ten Spalte keine Zeile gefunden wird, bei der Referenzwert > Wert der Spalte, gibt die Funktion FALSE zurück
    }
  }
  
  return(TRUE)  # Wenn alle Spalten die Bedingung erfüllen, gibt die Funktion TRUE zurück
}

check_vector_condition <- function(mat) {
  check <- c()
  for(i in nrow(mat)) {
    check <- c(check, check_matrix_condition(mat, i))
  }
  return(check)
}

maximal_creterion <- function(matrix){
  result_vec <- c()
  for(i in 1:nrow(matrix)) {
    result_vec <- c(result_vec, check_matrix_condition(matrix, i))
    
  }
  result <- which(result_vec)
  return(result)
  
}

generate_indicator_matrix <- function(mat) {
  # Bestimme die Anzahl der Reihen und Spalten
  m <- nrow(mat)
  n <- ncol(mat)
  print(mat)
  # Erstelle eine leere Matrix mit der gleichen Dimension, die mit 0 gefüllt ist
  result <- matrix(0, nrow = m, ncol = n)
  
  # Gehe Spalte für Spalte durch
  for (j in 1:n) {
    # Bestimme das größte Element in der j-ten Spalte
    max_value <- max(mat[, j])
    
    # Setze die Positionen des größten Werts in der entsprechenden Spalte auf 1
    for (i in 1:m) {
      if (mat[i, j] == max_value) {
        result[i, j] <- 1
        i <- m ################
      }
    }
  }
  
  return(result)
} #GPT

row_has_one <- function(mat) {
  # Überprüfe für jede Zeile, ob mindestens eine 1 vorhanden ist
  result <- apply(mat, 1, function(x) any(x == 1))
  
  return(result)
} #GPT

e_admissible_creterion <- function(matrix) {
  indicator <- generate_indicator_matrix(matrix)
  bool_vec <- row_has_one(indicator)
  result <- which(bool_vec)
  return(result)
  
}
####################### TEST ##################################################
