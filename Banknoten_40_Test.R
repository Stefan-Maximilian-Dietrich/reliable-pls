source("Alpha_cut/alpha_cut.R")
source("Alpha_cut/Hilfsfunktionen.R")


set.seed(2037420)


formula = target ~  Diagonal + Bottom + Length

data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'
data_frame$target <- as.numeric(data_frame$target == "genuine")

modell <- glm(formula = target ~  Diagonal + Bottom + Length, data = data_frame)

mu_priori_lower <- c(-45, -1, -1, -1)
mu_priori_lower <- c(-20, 1, 1, 1)
sigma_priori <-  rbind(beta0 = c(7,0,0,0),cbind(beta0 = c(0,0,0), cov(data_frame[c(7,5,2)]) ))
alpha = 0.8

for(i in 1:2) {
  data_frame_40 = data_frame[sample(nrow(data_frame), 40),]
  test_40 <- anti_join(data_frame, data_frame_40)
  unlabeld_40 <- data_frame_40[sample(nrow(data_frame_40), round(40*0.8)),]
  labled_40 <- anti_join(data_frame_40, unlabeld_40)
  
  Resultat_Liste <- list()
  Test_banknoten_40 <- tryCatch({
    alpha_cut(labeled_data = labled_40[c(1,7,5,2)], unlabeled_data = unlabeld_40[c(7,5,2)], test_data = test_40[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha) 
  }, error = function(e) {
    NA  
  })
  
  if(length(Test_banknoten_40) != 1) {
    data <- Accuracy_Result(Test_banknoten_40)
    ggplot(data = data, aes(x = Runde, y = Genauichkeit))+
      geom_line() +
      geom_point()
  }
  
  Resultat_Liste[[i]] <- Accuracy_Result(Test_banknoten_40)
}


