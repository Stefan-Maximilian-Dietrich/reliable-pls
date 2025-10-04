e_admissible_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  confusion <- test_confiusion(priori = NULL, train, test)
  result <- list(confusion)
  
  
  z = 1
  end <- nrow(unlabeld)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train, prioris)
    
    ##### Visualiserung
    
    ################
    cut_priori <- alpha_cut(marg_prioris, alpha)
    
    
    best_modell <- predict_best_model(cut_priori, train)
    
    #AusWAHL Index
    pseudo_data <- pseud_labeling(best_modell, train, unlabeld) #für die claculation
    pseudo_labled_data <-  predict_pseudo_labled_data(best_modell, unlabeld) #für die auswahl
    
    matrix <- decison_matrix(cut_priori, pseudo_data)
    
    ind_matrix <- generate_indicator_matrix(matrix)
    e_admissible <- e_admissible_creterion(ind_matrix)
    
    ###########
    train <- rbind(train, pseudo_labled_data[e_admissible, ])
    unlabeld <- unlabeld[-e_admissible, ]
    
    ########### Evaluation
    confusion <- test_confiusion(priori = best_modell$prior, train, test)
    for(w in 1:(length(e_admissible))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 

refernce_SSL <- function(train, unlabeld, test, priori = NULL) {
  
  confusion <- test_confiusion(priori = priori, train, test)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeld)) {
    if(is.null(priori)) {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
      
    } else {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), priori = priori)
    }
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
    pred_prob <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "prob")
    pred_class <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "class")
    prob_values <- sapply(seq_along(pred_class), function(i) {
      pred_prob[i, as.character(pred_class[i])]
    })
    unlabeld$nr <- 1:nrow(unlabeld)
    matrix <- cbind(prob_values, pred_target = names(prob_values), unlabeld)
    possible <- matrix[matrix$prob_values == max(matrix$prob_values), ]
    
    decision_index <- sample(possible$nr, 1)
    decision <- possible[possible$nr == decision_index,][, -c(1,3, ncol(possible))]
    names(decision) <- names(train)
    
    train <- rbind(train,decision )
    unlabeld <- unlabeld[unlabeld$nr != decision_index,]
    unlabeld$nr <- NULL
    
    if(is.null(priori)) {
      new_model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
      
    } else {
      new_model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = priori)
    }
    
    confusion <- test_confiusion(priori = priori, train, test)
    result <- c(result, list(confusion))  
    
    
  }
  return(result)
  
}

refernce_SSL_variance <- function(train, unlabeld, test, priori = NULL) { ####
  unlabeld$target <- NA
  
  confusion <- test_confiusion(priori = priori, train, test)
  result <- list(confusion)
  
  for(i in 1:nrow(unlabeld)) {
    if(is.null(priori)) {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
      
    } else {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), priori = priori)
    }
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
    pred_prob <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "prob")
    pred_class <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "class")
    unlabeld$target <- pred_class
    
    second_largest <- apply(pred_prob, 1, function(row) sort(row, decreasing = TRUE)[2])
    first_largest <- apply(pred_prob, 1, function(row) sort(row, decreasing = TRUE)[1])
    max_variance <- which.max(log(first_largest) - log(second_largest))
    
    
    train <- rbind(train,unlabeld[max_variance, ] )
    unlabeld <- unlabeld[-c(max_variance), ]
    
    confusion <- test_confiusion(priori = priori, train, test)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

refernce_SSL_entropy <- function(train, unlabeld, test, priori = NULL) { ####
  unlabeld$target <- NA
  
  confusion <- test_confiusion(priori = priori, train, test)
  result <- list(confusion)
  
  for(i in 1:nrow(unlabeld)) {
    if(is.null(priori)) {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
      
    } else {
      model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), priori = priori)
    }
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
    pred_prob <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "prob")
    pred_class <- predict(model, newdata = as.matrix(unlabeld[, -1]), type = "class")
    unlabeld$target <- pred_class
    
    entropy_values <- apply(pred_prob, 1, function(p) {
      p <- p[p > 0]
      return(-sum(p * log2(p)))
    })
    
    
    min_entropy <- which.min(entropy_values)
    
    
    train <- rbind(train,unlabeld[min_entropy, ] )
    unlabeld <- unlabeld[-c(min_entropy), ]
    
    confusion <- test_confiusion(priori = priori, train, test)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

maximal_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  confusion <- test_confiusion(priori = NULL, train, test)
  result <- list(confusion)
  
  z = 1
  end <- nrow(unlabeld)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train, prioris)
    
    ##### Visualiserung
    
    ################
    cut_priori <- alpha_cut(marg_prioris, alpha)
    
    
    best_modell <- predict_best_model(cut_priori, train)
    
    #AusWAHL Index
    pseudo_data <- pseud_labeling(best_modell, train, unlabeld) #für die claculation
    pseudo_labled_data <-  predict_pseudo_labled_data(best_modell, unlabeld) #für die auswahl
    
    matrix <- decison_matrix(cut_priori, pseudo_data)
    
    maximal <- maximalitaetskriterium(matrix)
    
    ###########
    train <- rbind(train, pseudo_labled_data[maximal, ])
    unlabeld <- unlabeld[-maximal, ]
    ########### Evaluation
    confusion <- test_confiusion(priori = best_modell$prior, train, test)
    for(w in 1:(length(maximal))) {
      
      result <- c(result, list(confusion))   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 

refernce_SL <- function(train, unlabeld, test, priori = NULL) {
  confusion <- test_confiusion(priori = priori, train, test)
  result <- rep(list(confusion), nrow(unlabeld) +1)
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
    M_MaxiMin <- M_MaxiMin_creterion(matrix)
    data_train <- rbind(data_train, data_pseudo[M_MaxiMin, ])
    data_unlabeled <- data_pseudo[-M_MaxiMin, ]
    confusion <- test_confiusion(data_train, data_test)
    for(w in 1:(length(M_MaxiMin))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  return(result)
} 

M_MaxiMax_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  confusion <- test_confiusion(priori = NULL, train, test)
  result <- list(confusion)
  
  z = 1
  end <- nrow(unlabeld)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train, prioris)
    
    ##### Visualiserung
    
    ################
    cut_priori <- alpha_cut(marg_prioris, alpha)
    
    
    best_modell <- predict_best_model(cut_priori, train)
    
    #AusWAHL Index
    pseudo_data <- pseud_labeling(best_modell, train, unlabeld) #für die claculation
    pseudo_labled_data <-  predict_pseudo_labled_data(best_modell, unlabeld) #für die auswahl
    
    matrix <- decison_matrix(cut_priori, pseudo_data)
    
    M_MaxiMax <- M_MaxiMax_creterion(matrix)
    
    ###########
    train <- rbind(train, pseudo_labled_data[c(M_MaxiMax), ])
    unlabeld <- unlabeld[-c(M_MaxiMax), ]
    ########### Evaluation
    confusion <- test_confiusion(priori = best_modell$prior, train, test)
    for(w in 1:(length(M_MaxiMax))) {
      
      result <- c(result, list(confusion))   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 
