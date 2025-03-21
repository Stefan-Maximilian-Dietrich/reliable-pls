######
#n_labled = 8
##n_unlabled = 25
#data = data_frame
#formula = target ~ Diagonal + Bottom + Length
#alpha = 0.8
#####
sample <- sampler_NB(n_labled,n_unlabled,data, formula)
train <- sample[[1]]
unlabeld <- sample[[2]]
test <-sample[[3]]

levels_present <- levels(data[,c(all.vars(formula)[1])]) 
prioris<- generate_priori_simplex(levels_present, step = 00.01)

z = 1
result <- NULL
end <- nrow(unlabeld)
while(z < end) {
  marg_prioris <- marginal_likelihoods(train, prioris)
  cut_priori <- alpha_cut(marg_prioris, alpha)
  best_modell <- predict_best_model(cut_priori, train)
  pseudo_data <- pseud_labeling(best_modell, train, unlabeld)
  matrix <- decison_matrix(cut_priori, pseudo_data)
  ind_matrix <- generate_indicator_matrix(matrix)
  e_admissible <- e_admissible_creterion(ind_matrix)
  train <- rbind(train, unlabeld[e_admissible,])
  unlabeld <- unlabeld[-e_admissible, ]
  ########### Evaluation
  confusion <- test_confiusion(priori = best_modell$prior, train, test)
  for(w in 1:(length(e_admissible))) {
    
    result <- c(result, confusion$overall[1]) 
    z = z + 1
  }
  #print(result)
  #plot(marg_prioris$genuine, marg_prioris$marg_likelis)
  
}
as.numeric(result)
