source("R/Alpha_cut/Function.R")



alpha_cut_dynamic <- function(labeled_data,
                      unlabeled_data,
                      test_data,
                      target,
                      glm_formula,
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
    t_ncols <- ncol(labeled_data)
    logistic_model <- glm(formula = formula, 
                          data = labeled_data[,-c(t_ncols)], 
                          family = "binomial")
    
    #Test des Modells
    scores = predict(logistic_model, newdata = test_data, type = "response") 
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
    
    #dynamischer Alpha cut 
    #print("################################")
    alpha <- test_acc * test_acc
    #print(paste("Alpha:", alpha))
    
    mu_priori_lower <- replace_na(logistic_model$coefficients, 0) - abs(replace_na(logistic_model$coefficients, 1))/2
    #print("mu_priori_lower")
    #print(mu_priori_lower)
    
    mu_priori_upper <- replace_na(logistic_model$coefficients, 0) - abs(replace_na(logistic_model$coefficients, 1))/2
    #print("mu_priori_upper")
    #print(mu_priori_upper)
    
    dim_soll <- length(mu_priori_upper)
    dim_haben <- length(summary(logistic_model)$coefficients[, "Std. Error"])
    dim_lücke <- dim_soll - dim_haben

    sigma_priori_vector <- c(summary(logistic_model)$coefficients[, "Std. Error"], rep(1, times = dim_lücke))
    #print(sigma_priori_vector)
      
    sigma_priori <- diag(sigma_priori_vector)
    #print("sigma_priori")
    #print(sigma_priori)
    #print("################################")
  
    
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
      core <- as.numeric(20)
      print(paste("Parallel", core))
      cl <- parallel::makeForkCluster(core)
      doParallel::registerDoParallel(cl)
      gamma <- foreach(i = 1:length(data_sets_pred), .combine = 'c') %dopar% {
        tryCatch({
          gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = glm_formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
        }, error = function(e) {
          Error <- readRDS("Errors.rds") 
          lenght <- length(Error)
          Error[[lenght + 1]] <- list(data = data_sets_pred[[i]], glm_formula = glm_formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
          saveRDS(Error, "Errors.rds") 
          
          return(0)
        })
      }
      parallel::stopCluster(cl)
    }
    
    if(!paralell) {
      gamma <- c()
      for(i in 1:length(data_sets_pred)) {
        g <- gamma_maximin_alpaC_addapter(data = data_sets_pred[[i]], glm_formula = glm_formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
        gamma <- c(gamma, g)
      }
    }
    
    winner <- which.max(unlist(gamma)) #
    
    
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
