library(stats)


test_1dim_function <- function(data, target, predict, ratio_ssl, ratio_test) {
  N <- nrow(data)
  key <- seq(N)
  key_lablet <- sample(x = key, size = round(N*ratio))
  labeld_data <- data[c(key_lablet),]
  unlabeled_data <- data[-c(key_lablet),]
  formal <- target ~ predict
  logistic_model <- glm(formula = formula, 
                        data = labeled_data, 
                        family = "binomial")
  
  predicted_target <- predict(logistic_model, 
                              newdata= unlabeled_data, 
                              type = "response")
  
  unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0) 
  
  
  return(logistic_model)
}


test_1dim_function(data = mtcars[c(1,8)], target = "vs", predict = "mpg", ratio = 0.5)




