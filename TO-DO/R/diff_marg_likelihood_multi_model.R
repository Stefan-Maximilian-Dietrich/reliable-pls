library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")


diff_marg_likelihood_multi_model <- function(labeled_data,
                                          unlabeled_data,
                                          test_data,
                                          target,# Variable 
                                          formulas, #Gleiche Modellannahme, aus der mehrere (verschiedene) Modelle werden, duch verscheiderne Formeln 
                                          glm_formula) {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_list(formulas)
  lapply(formulas, assert_formula)
  assert_formula(glm_formula)
  assert_character(target)
  
  n_imp = nrow(unlabeled_data) #Menge an Unlabeld Data, keine Test Daten 
  results = matrix(nrow = n_imp, ncol = 3) # Matrix wird vorbereitet 
  which_flip = seq(n_imp)
  for (i in seq(n_imp)) { #Alle unlabeld werden hinzugefügt 
    
    # fit model to labeled data
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    
    # predict on unlabeled data
    predicted_target <- predict(logistic_model,  #zahl zwischen 0 und 1 wird predicted
                                newdata= unlabeled_data, 
                                type = "response")
    
    # assign predicted (pseudo) labels to unlabeled data
    unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0) #1 und 0 sind die Lables  
    
    
    # create datasets that contain labeled data and one predicted instance each
    if(i >= 2){ # da i==1 betritt die Schleife das If-Statement in er ersten Itration niht 
      which_flip <- which_flip[-(winner)]
    }
    
    data_sets_pred_init = as.list(seq_along(which_flip))  # Liste mit allen elementn von 1 bis ... (loch das duch oben erzeugt wird wird aufgerückt)
    data_sets_pred = lapply(data_sets_pred_init, function(flip){
      new_data = rbind(labeled_data, unlabeled_data[flip,])
      new_data #Liste, jedes Element ist Labeld dataen Satz mit einer Unaldeld Instanz 
    })
    

    multi_bayes_crit <- lapply(data_sets_pred, function(data){ #Für jeden Datensatz in der liste 
       lapply(formulas, function(formula_i){ #für jeden formelin der liste 
        
        logistic_model <- glm(formula = formula_i,
                              data = data,
                              family = "binomial")
        logistic_model %>% get_log_marg_l()
      })
      })
    bayes_crit <- lapply(multi_bayes_crit, function(ppp){
      ppp %>% unlist %>% as.numeric %>% sum #Summieren über die Kreterien alle Möglichen Modelle (induziert duch verscheidenen Formeln)
    })

    crit_eval = unlist(bayes_crit)
    if(length(crit_eval) != 1)
      winner = which.max(crit_eval) #Datenpunkt mit höchster Utility wird gewählt 
    else
      winner = 1
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    # evaluate test error (on-the-fly inductive learning results)
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data) #wert der Vergleichbar ist 
    
    
    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    # store results
    results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
    unlabeled_data <- unlabeled_data[-winner,]
    
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model)
  
}


