source("R/Alpha_cut/Function.R")
#source("R/Alpha_cut/Function_MCMC.R")



multi_model_soft_revision <- function(labeled_data,
                                      unlabeled_data,
                                      test_data,
                                      target,
                                      glm_formula,
                                      formulas,
                                      mu_priori_lower,
                                      mu_priori_upper, 
                                      sigma_priori,
                                      alpha,
                                      paralell = TRUE) {
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  formula = glm_formula
  
  n_imp = nrow(unlabeled_data)
  results = matrix(nrow = n_imp, ncol = 3)
  which_flip = seq(n_imp)
  
  for (i in seq(n_imp)) {
    print(paste("Vortschritt:", i/n_imp*100, "%"))
    
    Zeit_Start <- Sys.time()
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
    
    saveRDS(data_sets_pred, "Last.rds") 
    
    if(paralell) {
      core <- as.numeric(min(20, parallel::detectCores() , length(data_sets_pred)*length(formulas)))
      print(paste("Parallel", core))
      cl <- parallel::makeForkCluster(core)
      doParallel::registerDoParallel(cl)
      
      gamma <- foreach(i = 1:length(data_sets_pred), .combine = rbind) %:% 
        foreach(j = 1:length(formulas), .combine = rbind) %dopar% {
          g <- gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = formulas[[j]], target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
          c(data= i,formula=j,g = g)
        }
      
      sum_values <- aggregate(g ~ data, data = gamma, FUN = max)
      winner <- sum_values$data[which.max(sum_values$g)]
      

    }
    View(gamma)
    
    
    if(!paralell) {
      gamma <- NULL
      for(i in 1:length(data_sets_pred)) {
        gamma_r <- NULL
        for(j in 1:length(formulas)) {
          g <- gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = formulas[[j]], target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
          gamma_r <- c(gamma_r, g)
          View(gamma_r)
        }
        gamma <- as.matrix(rbind(gamma, unlist(gamma_r)))
        View(gamma)
        
      }
      winner <- which.max(unlist(gamma)) #
      
    }
    
    
    
    # predict on it again and add to labeled data
    predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    # evaluate test error (on-the-fly inductive learning results)
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    
    # update labeled data
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    # store results
    results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
    unlabeled_data <- unlabeled_data[-winner,]
    
    Zeit_Ende <- Sys.time()
    print(paste("Für Schritt", i, "wurden", Zeit_Ende - Zeit_Start, "gebraucht"))
  }
  # get final model
  final_model <- logistic_model <- glm(formula = formula, 
                                       data = labeled_data, 
                                       family = "binomial")
  # return transductive results (labels) and final model
  list(results, final_model)
  
}