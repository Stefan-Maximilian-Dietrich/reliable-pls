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

  
  FUN <- function(mu) {
    return( function(theta) { mvnfast::dmvn(X = theta, mu = mu, sigma = sigma_priori)})
  }
  density <- lapply(mu, FUN)

  return(density)
}

likelihood_logistic <- function(X, response) {
  Y <- response
  
  logistic_function <- function(X, theta) {
    odds <- exp(X %*% theta) #Odds
    probability <- odds / (1 + odds)
    return(probability)
  }
  
  likelihood <- function(theta) {
    p <- logistic_function(X, theta)
    likelihood <- prod(Y * p + (1 - Y) * (1 - p))
    return(likelihood)
  }
  
  return(likelihood)
}

logistic_modell_function <- function(labeld_data, pseudo_data, formula) {
  data <- apply(pseudo_data,1, function(x) {rbind(labeld_data, x)})
  print(data)
  logistic_modells <- lapply(data, function(x) {glm(formula = formula, data = x, family = "binomial")})
  return(logistic_modells)
}

get_log_marg_l <- function(logistic_model) {
  # check if vcov-matrix has NA (in p>n scenarios)
  try({
    n_parameters <- length(logistic_model$coefficients)
    n_obs <- logistic_model$data %>% nrow
    cov_matrix <- vcov(logistic_model)
    if(sum(is.na(cov_matrix)) != nrow(cov_matrix)*ncol(cov_matrix)){
      not_nas <- vcov(logistic_model)[!is.na(vcov(logistic_model))]
      v_cov_matrix <- matrix(not_nas, ncol = sqrt(length(not_nas)))
      fisher_info <- v_cov_matrix %>% solve() 
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) - 0.25 * log(det(fisher_info))
    }else{
      cat("fisher info containing NAs exclusively. Replacing fisher info by 0 in log margL approx.")
      log_marg_l <- logLik(logistic_model) + n_parameters/2 * log(2*pi/n_obs) 
    }
    log_marg_l
  })
}

marginal_likelihood_function <- function(likelihood, priori) {
  g <- function(x) {
    return(likelihood(x)*priori(x))
  }
  h <- function(x) {
    return(log(g(x)))
  }
  h_neg <- function(x) {
    return(-h(x))
  }
  start <- rep(0, times = ncol(X)) #Starting point 
  x0 <- optim(par = start, fn = h_neg, method = "BFGS")$par  #Similar to the Newton method.
  hII_x0 <- hessian(h, x0) #Calculation of the Hessian matrix (i.e., the second derivative with respect to the point x0).
  
  marginal_likelyhood <- exp( h(x0) ) * sqrt( (2*pi)^2 / abs(det(hII_x0))) #Laplace Approximation
  return(marginal_likelyhood)
}

marginal_likelihood <- function(prioris, likelihood) {
  Marginal_list <- c()
  for(i in 1:length(prioris)) {
    new <- marginal_likelihood_function(likelihood, prioris[[i]])
    Marginal_list <- c(Marginal_list, new)
  }
  return(Marginal_list)
}

alpha_cut <- function(prioris, likelihood, alpha) {
  marginal <- marginal_likelihood(prioris, likelihood)
  max <- max(marginal)
  decision <- marginal >= alpha * max
  updated_prioris <- prioris[decision]
  return(updated_prioris)
}

ppp <- function(logistic_model, priori) {
  argmax <- summary(logistic_model)$coefficients[,1]
  print(argmax)
  result <- get_log_marg_l(logistic_model) + log(priori(argmax))
  return(result)
}

decision_matrix <- function(logistic_models, cut_prioris) {
  k <- length(logistic_models)
  l <- length(cut_prioris)
  print(k)
  print(l)
  matrix <- matrix(NA, nrow = k, ncol = l)
  for(i in 1:k) {
    for(j in 1:l) {
      print(paste(c(i,j)))
      matrix[i,j] <- ppp(logistic_models[[i]], cut_prioris[[j]])
    }
  }
  return(matrix)
}

check_matrix_condition <- function(mat, ref_col) {
  # Bestimme die Anzahl der Reihen und Spalten
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)
  
  # Überprüfe für jede Spalte, ob die Bedingung erfüllt ist
  for (j in 1:n_cols) {
    # Gehe jede Zeile durch und vergleiche den Wert in der Referenzspalte mit dem Wert in der j-ten Spalte
    if (!any(mat[, ref_col] > mat[, j])) {
      return(FALSE)  # Wenn in der j-ten Spalte keine Zeile gefunden wird, bei der Referenzwert > Wert der Spalte, gibt die Funktion FALSE zurück
    }
  }
  
  return(TRUE)  # Wenn alle Spalten die Bedingung erfüllen, gibt die Funktion TRUE zurück
}

check_vector_condition <- function(mat) {
  check <- c()
  for(i in length(mat)) {
    check <- c(check, check_matrix_condition(mat, i))
  }
  return(check)
}
maximal_creterion <- function(matrix){
  result <- c()
  for(i in 1:nrow(matrix)) {
    result <- c(result, check_matrix_condition(matrix, i))
    
  }
  return(result)
}

generate_indicator_matrix <- function(mat) {
  # Bestimme die Anzahl der Reihen und Spalten
  m <- nrow(mat)
  n <- ncol(mat)
  
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
  result <- row_has_one(indicator)
  return(result)
  
}

####################### TEST ##################################################

response <- data_frame[,1]
X <- as.matrix(data_frame[,-1])
X <- cbind(1,X)

prioris <- normal_radnom_spaced(10000, c(-3,-3,-3,-3,-3), c(3,3,3,3,3))
likelihood <- likelihood_logistic( X= X, response= response)

x <- c(0,0,0,0,0)
likelihood(x)*prioris[[1]](x)

marginal_likelihood_function(likelihood, prioris[[1]])
marginal_likelihood(prioris, likelihood)
prioris <- alpha_cut(prioris, likelihood, alpha = 0.1)

ppp(modell,  prioris[[]])

L <- list()
L[[1]] <- modell

mat <-(decision_matrix(modells, prioris ))

modells <- logistic_modell_function(data_frame, data_frame2, formula)

(e_admissible_creterion(mat))
maximal_creterion(mat)
check_vector_condition(mat)
####################### Expermiente #########################################