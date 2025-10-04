refernce_SSL <- function(data_train, data_unlabeled, data_test) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  for(i in 1:nrow(data_unlabeled)) {
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    probabilitys <- qda_predict_proba(data_train, data_new = data_unlabeled, eps = 1e-0) 
    selected <- SSL_prob_criterion(probabilitys)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    result <- c(result, list(confusion))
  }
  return(result)
}

refernce_SSL_variance <- function(data_train, data_unlabeled, data_test) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  for(i in 1:nrow(data_unlabeled)) {
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    probabilitys <- qda_predict_proba(data_train, data_new = data_unlabeled, eps = 1e-0) 
    selected <- SSL_var_criterion(probabilitys)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    result <- c(result, list(confusion))
  }
  return(result)
}

refernce_SSL_entropy <- function(data_train, data_unlabeled, data_test) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  for(i in 1:nrow(data_unlabeled)) {
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    probabilitys <- qda_predict_proba(data_train, data_new = data_unlabeled, eps = 1e-0) 
    selected <- SSL_entropy_criterion(probabilitys)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    result <- c(result, list(confusion))
  }
  return(result)
}

refernce_SL <- function(data_train, data_unlabeled, data_test) {
  confusion <- test_confiusion(data_train, data_test)
  result <- rep(list(confusion), nrow(data_unlabeled) +1)
  return(result)
}

maximal_SSL <- function(priors, data_train, data_unlabeled, data_test, alpha) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  z = 1
  end <- nrow(data_unlabeled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(data_train, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    posteriors <- Posterior(data_train, cut_prioris)
    matrix <- PPP_matrix(data_train, data_pseudo, posteriors) 
    selected <- maximalitaetskriterium(matrix)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    for(w in 1:(length(selected))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  return(result)
} 

M_MaxiMin_SSL <- function(priors, data_train, data_unlabeled, data_test, alpha) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  z = 1
  end <- nrow(data_unlabeled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(data_train, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    posteriors <- Posterior(data_train, cut_prioris)
    matrix <- PPP_matrix(data_train, data_pseudo, posteriors) 
    selected <- M_MaxiMin_creterion(matrix)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    for(w in 1:(length(selected))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  return(result)
} 

e_admissible_SSL <- function(priors, data_train, data_unlabeled, data_test, alpha) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  z = 1
  end <- nrow(data_unlabeled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(data_train, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    posteriors <- Posterior(data_train, cut_prioris)
    matrix <- PPP_matrix(data_train, data_pseudo, posteriors) 
    selected <- e_admissible_creterion(matrix)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    for(w in 1:(length(selected))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  return(result)
} 

M_MaxiMax_SSL <- function(priors, data_train, data_unlabeled, data_test, alpha) {
  confusion <- test_confiusion(data_train, data_test)
  result <- list(confusion)
  z = 1
  end <- nrow(data_unlabeled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(data_train, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    data_pseudo <- qda_predict_class(data_train, data_new = data_unlabeled) 
    posteriors <- Posterior(data_train, cut_prioris)
    matrix <- PPP_matrix(data_train, data_pseudo, posteriors) 
    selected <- M_MaxiMax_creterion(matrix)
    data_train <- rbind(data_train, data_pseudo[selected, ])
    data_unlabeled <- data_pseudo[-selected, ]
    confusion <- test_confiusion(data_train, data_test)
    for(w in 1:(length(selected))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  return(result)
} 
