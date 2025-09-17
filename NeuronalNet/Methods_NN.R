e_admissible_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha) {
  
  confusion <- test_confiusion(train_scaled, test_scaled)
  #print(confusion)
  result <- list(confusion)
  
  
  z = 1
  end <- nrow(unlabeled_scaled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train_scaled, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled)
    
    ######
    ppp_m <- PPP_matrix(cut_prioris, train_scaled, pseudolabeled_scaled) 
    matrix <-  xtabs(log_PPP ~ psID+priorID, data = ppp_m)
    e_admissible <- e_admissible_creterion(matrix)
    
    ###########
    #print(pseudolabeled_scaled[e_admissible, ])
    #print(unlabeled_scaled[e_admissible, ])
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[e_admissible, ])
    unlabeled_scaled <- pseudolabeled_scaled[-e_admissible, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled)
    #print(confusion)
    
    
    for(w in 1:(length(e_admissible))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
    
    
  }
  
  return(result)
  
} 

refernce_SL <- function(train_scaled, unlabeled_scaled, test_scaled) {
  confusion <- test_confiusion(train_scaled, test_scaled)
  result <- rep(list(confusion), nrow(unlabeled_scaled) +1)
  return(result)
}
refernce_SSL <- function(train_scaled, unlabeled_scaled, test_scaled) {
  
  
  confusion <- test_confiusion(train_scaled, test_scaled)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
    
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled)
    max_prob <- which.max(apply(prob_prediction, 1, max))
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled)
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[max_prob, ])
    unlabeled_scaled <- pseudolabeled_scaled[-max_prob, ]
    
    confusion <- test_confiusion(train_scaled, test_scaled)
    result[[i+1]] <- confusion
    
  }
  return(result)
  
}


