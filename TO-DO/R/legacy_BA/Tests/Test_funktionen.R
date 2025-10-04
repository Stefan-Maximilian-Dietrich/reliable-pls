Test_acc <- function(Data, modell) {
  Data$prediction <- as.numeric(predict(modell, Data, type = "response") > 0.5)
  Data$result <- as.numeric(Data$prediction == Data$y)
  result <- sum(Data$result) / length(Data$result)
  return(result)
}

Test_acc_diff <- function(Test_Data, unlabeld_data, labeld_data) {
  modell_ohne <- glm(formula = formula, data = labeld_data, family = "binomial")
  ohne <- Test_acc(Data = Test_Data, modell = modell_ohne)
  print(ohne)

  pseudo_data <- unlabeld_data
  pseudo_data$y <- as.numeric(predict(modell_ohne, pseudo_data, type = "response") > 0.5)
  print(rbind(labeld_data, pseudo_data))
  modell_pred <- glm(formula = formula, data = rbind(labeld_data, pseudo_data), family = "binomial")
  pred <- Test_acc(Test_Data, modell_pred)
  
  modell_hypo <- glm(formula = formula, data = rbind(labeld_data, unlabeld_data), family = "binomial")
  hypo <- Test_acc(Test_Data, modell_hypo)
  
  result <- c(ohne, pred, hypo)
  names(result) <- c("ohne", "pseudo", "hypothetisch")
  return(result)
}