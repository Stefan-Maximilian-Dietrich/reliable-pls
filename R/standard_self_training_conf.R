library(dplyr)
library(checkmate,asserthat)



#Funktion führt standart self training mit Conf als auswahlkriterium duch 
standard_self_training_conf <- function(labeled_data,
                                   unlabeled_data,
                                   test_data,
                                   target,
                                   glm_formula) {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  n_imp = nrow(unlabeled_data)
  results = matrix(nrow = n_imp, ncol = 3)
  which_flip = seq(n_imp) #wieos ist das hier? 

  for (i in seq(n_imp)) {
    
    
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    
    #choose instance whose prediction has most CONFIDENCE (as opposed to certainty)
    response_preds <- predict(logistic_model, newdata= unlabeled_data, type = "response") 
    abs_confidence <- ifelse(response_preds > 0.5, abs(1 - response_preds), abs(0 - response_preds)) #Abstand zum Optimum ist entscheiden, es wird eine Datenpunkt gewählt der Möglichs nah an einem echten Labele ist 
    winner <- which.min(abs_confidence)
    # min.col <- function(m, ...) max.col(-m, ...)
    # winner <- min.col(matrix(abs_confidence, nrow = 1), ties.method = "random") # make sure indices are returned randomly in case of ties
    # if(length(winner) >= 2)
    
    
    # predict it
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    

    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    
    # evaluate test error (on-the-fly inductive learning results)
    logistic_model <- glm(formula = formula, data = labeled_data, family = "binomial") # refit model with added label
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    # store results
    results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
    unlabeled_data <- unlabeled_data[-winner,]
  }
  
  # return transductive results (labels) and final model
  list(results, logistic_model)
  
}


