source("Function_e_ad.R")


#see algo 1 in paper 
# 
e_admissible <- function(labeled_data,
                         unlabeled_data,
                         test_data,
                         target,
                        glm_formula,
                        prioris,
                        log_likelihood,
                        alpha,
                        boundary,
                        creterion = "e-admissible") {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  n_imp = nrow(unlabeled_data)
  results = matrix(nrow = n_imp, ncol = 3)
  
  for (i in seq(n_imp)) {
    print(paste("Unlabled data:", nrow(unlabeled_data), "Round:", i ))
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
    
    
    # alpha cut 
    #print(paste(c("do:", "likelihood")))
    
    log_likelihood <- get_log_likelihood(labeled_data, glm_formula, target)
    
    #print(paste(c("do:", "alpha cut ")))
    prioris_cut <- alpha_cut(prioris, log_likelihood, alpha, boundary)
    print(paste("Prioris:", length(prioris), "Prisoris after Cut with alpha =", alpha, ":", length(prioris_cut), "Ratio:", length(prioris_cut)/ length(prioris) ))
    
    # creat a logistic Modell for every possinle decision
    #print(paste(c("do:", "logistic modells")))
    
    logistic_modells <- logistic_modell_function(labeled_data, unlabeled_data,glm_formula )
    
    #create Matrix with ppp of every combination of priori and likelihood
    dims <- c(ncol(labeled_data) -1, nrow(labeled_data))
    matrix <- decision_matrix(dims, logistic_modells, prioris_cut)
    

    # Selec action whitch forfill the creterion
    if(creterion == "e-admissible") {
      select <- e_admissible_creterion(matrix)
    }
    
    if(creterion == "maximal") {
      select <- maximal_creterion(matrix)
    }
    
    #print(paste(("do:", "new labelde data")))
    
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[select,], type = "response")
    new_labeled_obs <- unlabeled_data[select,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0) 
    
    
    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    
    # evaluate test error (on-the-fly inductive learning results)
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    
    # print result
    #print(paste(c("do:", "save results")))
    
    for(j in 1:length(select)) {
      results[i+j-1,] <- c(j, new_labeled_obs[j, ][, c(target)], test_acc) %>% unlist()
    }
    
    unlabeled_data <- unlabeled_data[-select,]
    print(paste("Number of choosen actions", length(select)))
    if( nrow(unlabeled_data) == 0) {
      i = n_imp
    }
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model)
  
}


