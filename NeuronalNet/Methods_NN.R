e_admissible_SSL <- function(prioris, train_scaled, unlabeled_scaled, test_scaled, alpha) {
  
  confusion <- test_confiusion(train_scaled, test_scaled)
  
  result <- list(confusion)
  
  
  z = 1
  end <- nrow(unlabeled_scaled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train_scaled, priors)
    cut_prioris <- alpha_cut(marg_prioris, alpha)
    
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled)

    ######
    ppp_m <- PPP_matrix(priors, train_scaled, pseudolabeled_scaled) 
    matrix <-  xtabs(log_PPP ~ psID+priorID, data = ppp_m)
    e_admissible <- e_admissible_creterion(ind_matrix)
    
    ###########
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[e_admissible, ])
    unlabeled_scaled <- pseudolabeled_scaled[-e_admissible, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled)
    for(w in 1:(length(e_admissible))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 
