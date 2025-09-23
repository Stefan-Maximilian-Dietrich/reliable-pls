e_admissible_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i) {
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
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

M_MaxiMin_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i) {
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
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
    
    M_MaxiMin <- M_MaxiMin_creterion(matrix)
    
    
    ###########
    #print(pseudolabeled_scaled[e_admissible, ])
    #print(unlabeled_scaled[e_admissible, ])
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[M_MaxiMin, ])
    unlabeled_scaled <- pseudolabeled_scaled[-M_MaxiMin, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled)
    #print(confusion)
    
    
    for(w in 1:(length(M_MaxiMin))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
    
    
  }
  
  return(result)
  
} 

refernce_SL <- function(train_scaled, unlabeled_scaled, test_scaled, i) {
  confusion <- test_confiusion(train_scaled, test_scaled, i)
  result <- rep(list(confusion), nrow(unlabeled_scaled) +1)
  return(result)
}

refernce_SSL <- function(train_scaled, unlabeled_scaled, test_scaled, i) {
  
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
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

#untested
refernce_SSL_variance <- function(train_scaled, unlabeled_scaled, test_scaled, i) { ####

  confusion <- test_confiusion(train_scaled, test_scaled, i)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
   
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled)
    
    
    second_largest <- apply(prob_prediction, 1, function(row) sort(row, decreasing = TRUE)[2])
    first_largest <- apply(prob_prediction, 1, function(row) sort(row, decreasing = TRUE)[1])
    max_variance <- which.max(log(first_largest) - log(second_largest))
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[max_variance, ] )
    unlabeled_scaled <- pseudolabeled_scaled[-c(max_variance), ]
    
    confusion <- test_confiusion(train_scaled, test_scaled)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

#untested
refernce_SSL_entropy <- function(train_scaled, unlabeled_scaled, test_scaled, i) {
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled)
    
    

    entropy_values <- apply(prob_prediction, 1, function(p) {
      p <- p[p > 0]
      return(-sum(p * log2(p)))
    })
    
    
    min_entropy <- which.min(entropy_values)
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[min_entropy, ] )
    unlabeled_scaled <- pseudolabeled_scaled[-c(min_entropy), ]
    
    confusion <- test_confiusion(train_scaled, test_scaled)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

#untested
maximal_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i) {
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
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
    
    
    maximal <- maximalitaetskriterium(matrix)
    
    ###########
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[M_MaxiMin, ])
    unlabeled_scaled <- pseudolabeled_scaled[-M_MaxiMin, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled)
    #print(confusion)
    
    for(w in 1:(length(maximal))) {
      
      result <- c(result, list(confusion))   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 

#untested
M_MaxiMax_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i) {
  
  confusion <- test_confiusion(train_scaled, test_scaled, i)
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
    
    
    M_MaxiMax <- M_MaxiMax_creterion(matrix)
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[M_MaxiMin, ])
    unlabeled_scaled <- pseudolabeled_scaled[-M_MaxiMin, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled)
    #print(confusion)
    
    for(w in 1:(length(M_MaxiMax))) {
      
      result <- c(result, list(confusion))   ##### wird list mir allen möglichen wert
      z = z + 1
    }
    #print(result)
    #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
    
  }
  
  return(result)
  
} 

