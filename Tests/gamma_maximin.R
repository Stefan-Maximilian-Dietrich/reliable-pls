library(dplyr)
library(checkmate,asserthat)
source("R/Function_m.R")


gamma_maximin  <- function( labeled_data,
                            unlabeled_data,
                            test_data,
                            target,
                            glm_formula,
                            mu_priori_Lower, 
                            mu_priori_upper,
                            sigma_priori) {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  assert_numeric(mu_priori_Lower)
  assert_numeric(mu_priori_upper)
  
  
  formula = glm_formula
  
  n_imp = nrow(unlabeled_data)
  results <- list()
  which_flip = seq(n_imp)
  
  for (i in seq(n_imp)) {
    
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
    
    gamma <- function(x) {
      return(gamma_maximin_function(data_fram = x, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori))
    }
    
    gamma_maximin <- lapply(data_sets_pred, gamma) 
    winner <- which.max(unlist(gamma_maximin)) 
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    
    # evaluate test error (on-the-fly inductive learning results)
    logistic_model <- glm(formula = formula, data = labeled_data, family = "binomial") # refit model with added label
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    print("store results")
    # store results
    results[[i]] <- list(summary(logistic_model)$aic, test_acc)
    unlabeled_data <- unlabeled_data[-winner,]
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  return(results)
  
}

