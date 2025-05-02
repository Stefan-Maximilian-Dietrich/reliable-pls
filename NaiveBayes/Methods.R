e_admissible_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  result <- acc_new
  
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
      
      result <- c(result, confusion$overall[1])   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(as.numeric(result))
  
} 

maximal_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  result <- acc_new
  
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
      
      result <- c(result, confusion$overall[1])   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(as.numeric(result))
  
} 

refernce_SSL <- function(train, unlabeld, test, priori = NULL) {
  
  if(is.null(priori)) {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
    
  } else {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = priori)
  }
  
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  acc <- acc_new
  
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
    
    pred_test <- predict(new_model, newdata = as.matrix(test[, -1]), type = "class")
    acc_new <- sum((pred_test == test$target))/length(pred_test)
    acc <- c(acc, acc_new)
    
  }
  return(acc)
  
}

refernce_SL <- function(train, unlabeld, test, priori = NULL) {
  if(is.null(priori)) {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
    
  } else {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), priori = priori)
  }
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  acc <- rep(acc_new, times= nrow(unlabeld) + 1)
  return(acc)
}

M_MaxiMin_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  result <- acc_new
  
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
    
    M_MaxiMin <- M_MaxiMin_creterion(matrix)
    
    ###########
    train <- rbind(train, pseudo_labled_data[c(M_MaxiMin), ])
    unlabeld <- unlabeld[-c(M_MaxiMin), ]
    ########### Evaluation
    confusion <- test_confiusion(priori = best_modell$prior, train, test)
    for(w in 1:(length(M_MaxiMin))) {
      
      result <- c(result, confusion$overall[1])   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(as.numeric(result))
  
} 

M_MaxiMax_SSL <- function(prioris, train, unlabeld, test, alpha) {
  
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  result <- acc_new
  
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
      
      result <- c(result, confusion$overall[1])   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(as.numeric(result))
  
} 