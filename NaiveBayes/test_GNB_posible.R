


# Vergleich mimiumm Viable modell vs. 50/50 split 
# wenn große steigerung NB SSL möglich 
#formula <- generate_formula(data, "Type")

#MIN
min <- minimum_viable(data, "target")
data_samp <- sampler_NB_up(n_labled = min[1], n_unlabled = min[2] , data = data, formula = formula)
train <- data_samp[[1]]
test <- data_samp[[3]]
priori <- NULL
modell_min <- test_confiusion(priori = priori, train =train , test = test )

#50/50
data_samp <- sampler_NB_up(n_labled = round(nrow(data)/2), n_unlabled = 0 , data = data, formula = formula)
train <- data_samp[[1]]
test <- data_samp[[3]]
priori <- NULL
modell_50 <- test_confiusion(priori = priori, train =train , test = test )


modell_min$overall[1]
modell_50$overall[1]




