e_admissible_SSL <- function(train, unlabeld, test, alpha) {
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  prioris<- generate_priori_simplex(levels_present, step = 0.01)
  
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target))
  pred_test <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  acc_new <- sum((pred_test == test$target))/length(pred_test)
  result <- acc_new
  
  z = 1
  end <- nrow(unlabeld)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train, prioris)
    
    ##### Visualiserung
    p <- ggplot(marg_prioris, aes(x = genuine, y = marg_likelis)) +
      geom_point() +  # Scatter plot
      geom_line() +   # Line plot
      geom_hline(yintercept =(1+alpha) * max(marg_prioris$marg_likelis), linetype = "dashed", color = "red") +  # Horizontal line at y = 0
      labs(title = "Simple ggplot Example", x = "X-axis", y = "Y-axis") +
      theme_minimal()  # Use a minimal theme
    print(p)
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
