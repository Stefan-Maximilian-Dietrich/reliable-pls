tan_SSL <- function(prioris, dt_train, dt_unlabled, dt_test, alpha, criterion) {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "TAN", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
    
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence(structure = "TAN", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris) 
    dt_pseudolabelt <- give_pseudo_label(priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    exp_utility_table <- get_Expected_utilitys(structure = "TAN", dt_pseudolabelt, dt_train, cut_priori, prioris) 
    
    if(criterion == "E_admissible"){
      action <- E_admissible(exp_utility_table)
    }
    if(criterion == "maximal"){
      action <- maximal(exp_utility_table)
    }
    if(criterion == "M_MaxiMin"){
      action <- M_MaxiMin(exp_utility_table)
    }
    if(criterion == "M_MaxiMax"){
      action <- M_MaxiMax(exp_utility_table)
    }
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "TAN",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)

    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }

  }
  
  return(as.numeric(result))
  
} 

###############################################################################

nb_SSL_E_admissible <- function(prioris, dt_train, dt_unlabled, dt_test, alpha) {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    exp_utility_table <- get_Expected_utilitys(structure = "NB", dt_pseudolabelt[, prob:= NULL], dt_train, cut_priori, prioris) 
    action <- E_admissible(exp_utility_table)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL_maximal <- function(prioris, dt_train, dt_unlabled, dt_test, alpha) {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    exp_utility_table <- get_Expected_utilitys(structure = "NB", dt_pseudolabelt, dt_train, cut_priori, prioris) 
    action <- maximal_a(exp_utility_table)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL_M_MaxiMin <- function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    exp_utility_table <- get_Expected_utilitys(structure = "NB", dt_pseudolabelt[, prob:= NULL], dt_train, cut_priori, prioris) 
    action <- M_MaxiMin_a(exp_utility_table)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL_M_MaxiMax <- function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    exp_utility_table <- get_Expected_utilitys(structure = "NB", dt_pseudolabelt[, prob:= NULL], dt_train, cut_priori, prioris) 
    action <- M_MaxiMax_a(exp_utility_table)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL <- function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
  
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    dt_pseudolabelt_all <- give_pseudo_label_all(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    action <- SSL_a(dt_pseudolabelt_all)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL_variance <- function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    dt_pseudolabelt_all <- give_pseudo_label_all(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    action <- SSL_variance_a(dt_pseudolabelt_all)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SSL_entropy <- function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence("NB", copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris[, prioID := 1:.N]) 
    dt_pseudolabelt <- give_pseudo_label(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    
    dt_pseudolabelt_all <- give_pseudo_label_all(structure = "NB", priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    action <- SSL_entropy_a(dt_pseudolabelt_all)
    
    ######################################################
    dt_train <- rbind(dt_train[, datID:=NULL], dt_pseudolabelt[action][, UdatID:=NULL][, datID:=NULL])
    dt_unlabled <- dt_unlabled[-action]
    dt_unlabled[, UdatID:= NULL]
    ########### Evaluation
    #print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(structure = "NB",priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} 

nb_SL <-function(prioris, dt_train, dt_unlabled, dt_test, alpha)  {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(structure = "NB", priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  
  for(w in 1:end) {
    result <- c(result, acc_new) 
  }
  return(as.numeric(result))
  
}