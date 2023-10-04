library(dplyr)
library(checkmate,asserthat)
library(foreach)
library(doParallel)
library(numDeriv)
library(mvtnorm)
library(ggplot2)
library(pracma)
library(nloptr)
library(tidyverse)
library(MixGHD)



source("Alpha_cut/Function.R")

alpha_cut <- function(labeled_data,
                      unlabeled_data,
                      test_data,
                      target,
                      glm_formula,
                      mu_priori_lower,
                      mu_priori_upper, 
                      sigma_priori,
                      alpha) {
  
  print("Starte Prozess")
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  print("check compleat")
  
  formula = glm_formula
  
  n_imp = nrow(unlabeled_data)
  results = list()
  which_flip = seq(n_imp)
  
  for (i in seq(n_imp)) {
    print(paste("Iteration:", i))
    
    print(formula)
    print(labeled_data)
    # fit model to labeled data
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")

    # predict on unlabeled data
    predicted_target <- predict(logistic_model, 
                                newdata= unlabeled_data, 
                                type = "response")
    # assign predicted (pseudo) labels to unlabeled data
    unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  

    
    # create datasets that contain labeled data and one predicted instance each
    if(i >= 2){
      which_flip <- which_flip[-(winner)]
    }
    data_sets_pred_init = as.list(seq_along(which_flip)) 
    data_sets_pred = lapply(data_sets_pred_init, function(flip){
      new_data = rbind(labeled_data, unlabeled_data[flip,])
      new_data 
    })
    
    ##### 2funktionen
    #core <- as.numeric(length(data_sets_pred))
    #print(core)
    #print("Parallel")
    #cl <- parallel::makeForkCluster(core)
    #doParallel::registerDoParallel(cl)
    #gamma <- foreach(i = 1:length(data_sets_pred), .combine = 'c') %dopar% {
    #  gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
    #}
    #parallel::stopCluster(cl)
    ###################
    

    gamma <- c()
    for(i in 1:length(data_sets_pred)) {
      g <- gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
      gamma <- c(gamma, g)
    }
    

    winner <- which.max(unlist(gamma)) #
    
    print("Gewinner:")
    print(winner)
    
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    # evaluate test error (on-the-fly inductive learning results)
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    print(paste("Testresultat:", test_acc))
    
    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    # store results
    results[[i]] <- list(Test = test_acc, Neun = new_labeled_obs, Utility = cbind(unlabeled_data, gamma))
    unlabeled_data <- unlabeled_data[-winner,]
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  return(results)
}

