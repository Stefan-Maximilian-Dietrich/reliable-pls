data_samp <- sampler_NB_up(6, 15, data, formula)
dt_train <- as.data.table(discretize_rec_min_ent(data_samp[[1]]))
dt_test <- as.data.table(discretize_rec_min_ent_test(data_samp[[1]], data_samp[[3]]))
dt_unlabled <- as.data.table(discretize_rec_min_ent_test(data_samp[[1]], data_samp[[2]]))

vergleich <- copy(dt_unlabled)
prioris <- as.data.table(gerate_priori_simplex_rec(levels = unique(dt_train$target), 100))

alpha <- 0.9
criterion <- "M_MaxiMin"

tan_SSL <- function(prioris, train, unlabeld, test, alpha, criterion) {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
    
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence(copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris) 
    dt_pseudolabelt <- give_pseudo_label(priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    exp_utility_table <- get_Expected_utilitys(dt_pseudolabelt, dt_train, cut_priori, prioris) 
    
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
    print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)

    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }

  }
  
  return(as.numeric(result))
  
} 

nb_SSL <- function(prioris, train, unlabeld, test, alpha, criterion) {
  first_prior <- as.data.table(table(dt_train$target)/nrow(dt_train))
  colnames(first_prior) <- c("c", "prio")
  predictions <- predict_tan(priori = first_prior, copy(dt_train),copy(dt_test))
  acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
  result <- acc_new
  
  z = 1
  end <- nrow(dt_unlabled)
  while(z <= end) {
    evidence <- get_Evidence(copy(dt_train), prioris)
    cut_priori <- do_alpha_cut(evidence, alpha)
    best_priori <- get_max_priori(cut_priori, prioris) 
    dt_pseudolabelt <- give_pseudo_label(priori = best_priori, dt_train = dt_train, dt_unlabled = copy(dt_unlabled))
    ######################################################
    exp_utility_table <- get_Expected_utilitys(dt_pseudolabelt, dt_train, cut_priori, prioris) 
    
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
    print(dt_pseudolabelt[action])
    
    predictions <- predict_tan(priori = best_priori, copy(dt_train),copy(dt_test))
    acc_new <- sum((predictions$target == dt_test$target))/length(dt_test$target)
    
    for(w in 1:(length(action))) {
      result <- c(result, acc_new) 
      z = z + 1
    }
    
  }
  
  return(as.numeric(result))
  
} ####
