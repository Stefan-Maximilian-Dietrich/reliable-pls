e_admissible_SSL <- function(priors, tain_data, unlabeled_data, test_data, alpha, class_col) {
  confusion <- evaluate_gnb_with_confusion(train_data, test_data, class_col)
  result <- list(confusion)
  z = 1
  end <- nrow(unlabeled_data)
  while(z <= end) {
    evidence_df <- evaluate_priors(tain_data, class_col, priors)
    cut_prioris <- alpha_cut_catteneo(evidence_df , priors, alpha)
    pseudo_data <- pseudolabeled(train_data, unlabeled_data, class_col ) 
    matrix <- compute_exact_ppp_matrix(train_data, pseudo_data, cut_prioris, class_col) 
    e_admissible <- e_admissible_creterion(matrix)
    train_data <- rbind(tain_data, pseudo_data[e_admissible, ])
    unlabeled_data <- pseudo_data[-e_admissible, ]
    confusion <- evaluate_gnb_with_confusion(train_data, test_data, class_col)
  
    for(w in 1:(length(e_admissible))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
  }
  
  return(result)
} 
######### Tests
priors <- sample_priors(n = 20, d = 4)
sample_data <- sampler_NB_up(data = iris, formula = Species ~  Sepal.Length + Sepal.Width + Petal.Length + Petal.Width    , n_labled = 50, n_unlabled = 10)
tain_data <- sample_data[[1]]
unlabeled_data <- sample_data[[2]]
test_data <- sample_data[[3]]
alpha = 0.5
class_col = "Species"
A <- e_admissible_SSL(priors, tain_data, unlabeled_data, test_data, alpha, class_col)
A[[1]]$overall["Accuracy"]
A[[10]]$overall["Accuracy"]


########## Test Ende 












M_MaxiMin_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i, h) {
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  #print(confusion)
  result <- list(confusion)
  
  
  z = 1
  end <- nrow(unlabeled_scaled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train_scaled, priors, h, d, K)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h, d, K)
    
    ######
    ppp_m <- PPP_matrix(cut_prioris, train_scaled, pseudolabeled_scaled,h,d,K) 
    matrix <-  xtabs(log_PPP ~ psID+priorID, data = ppp_m)
    
    M_MaxiMin <- M_MaxiMin_creterion(matrix)
    
    
    ###########
    #print(pseudolabeled_scaled[e_admissible, ])
    #print(unlabeled_scaled[e_admissible, ])
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[M_MaxiMin, ])
    unlabeled_scaled <- pseudolabeled_scaled[-M_MaxiMin, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
    #print(confusion)
    
    
    for(w in 1:(length(M_MaxiMin))) {
      result <- c(result, list(confusion))  
      z = z + 1
    }
    
    
  }
  
  return(result)
  
} 

refernce_SL <- function(train_scaled, unlabeled_scaled, test_scaled, i, h) {
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  result <- rep(list(confusion), nrow(unlabeled_scaled) +1)
  return(result)
}

refernce_SSL <- function(train_scaled, unlabeled_scaled, test_scaled, i, h) {
  
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
    
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled, h,d,K)
    max_prob <- which.max(apply(prob_prediction, 1, max))
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h,d,K)
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[max_prob, ])
    unlabeled_scaled <- pseudolabeled_scaled[-max_prob, ]
    
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
    result[[i+1]] <- confusion
    
  }
  return(result)
  
}

refernce_SSL_variance <- function(train_scaled, unlabeled_scaled, test_scaled, i, h) { ####
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
    
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled,  h,d,K)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h,d,K)
    
    
    second_largest <- apply(prob_prediction, 1, function(row) sort(row, decreasing = TRUE)[2])
    first_largest <- apply(prob_prediction, 1, function(row) sort(row, decreasing = TRUE)[1])
    max_variance <- which.max(log(first_largest) - log(second_largest))
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[max_variance, ] )
    unlabeled_scaled <- pseudolabeled_scaled[-c(max_variance), ]
    
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

refernce_SSL_entropy <- function(train_scaled, unlabeled_scaled, test_scaled, i, h) {
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  result <- list(confusion)
  
  
  for(i in 1:nrow(unlabeled_scaled)) {
    prob_prediction <- predict_pseudo_labels_prob(train_scaled, unlabeled_scaled,  h,d,K)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h,d,K)
    
    
    
    entropy_values <- apply(prob_prediction, 1, function(p) {
      p <- p[p > 0]
      return(-sum(p * log2(p)))
    })
    
    
    min_entropy <- which.min(entropy_values)
    
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[min_entropy, ] )
    unlabeled_scaled <- pseudolabeled_scaled[-c(min_entropy), ]
    
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
    result <- c(result, list(confusion))
    
  }
  return(result)
  
}

maximal_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i, h) {
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  #print(confusion)
  result <- list(confusion)
  z = 1
  end <- nrow(unlabeled_scaled)
  
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train_scaled, priors, h, d, K)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h,d,K)
    
    ######
    ppp_m <- PPP_matrix(cut_prioris, train_scaled, pseudolabeled_scaled,h,d,K) 
    matrix <-  xtabs(log_PPP ~ psID+priorID, data = ppp_m)
    
    
    maximal <- maximalitaetskriterium(matrix)
    
    ###########
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[maximal, ])
    unlabeled_scaled <- pseudolabeled_scaled[-maximal, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
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

M_MaxiMax_SSL <- function(priors, train_scaled, unlabeled_scaled, test_scaled, alpha, i, h) {
  d <- ncol(train_scaled) -1 
  K <- length(unique(train_scaled$target))
  
  confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
  #print(confusion)
  result <- list(confusion)
  z = 1
  end <- nrow(unlabeled_scaled)
  while(z <= end) {
    marg_prioris <- marginal_likelihoods(train_scaled, priors, h, d, K)
    cut_prioris <- alpha_cut(marg_prioris, alpha, priors)
    pseudolabeled_scaled <- predict_pseudo_labels(train_scaled, unlabeled_scaled, h,d,K)
    
    ######
    ppp_m <- PPP_matrix(cut_prioris, train_scaled, pseudolabeled_scaled,h,d,K) 
    matrix <-  xtabs(log_PPP ~ psID+priorID, data = ppp_m)
    
    
    M_MaxiMax <- M_MaxiMax_creterion(matrix)
    
    train_scaled <- rbind(train_scaled, pseudolabeled_scaled[M_MaxiMax, ])
    unlabeled_scaled <- pseudolabeled_scaled[-M_MaxiMax, ]
    
    ########### Evaluation
    confusion <- test_confiusion(train_scaled, test_scaled, i, h, d, K)
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

