train <- sampler_NN_up(100,8, data, formula)[[1]]
priori <- c(0.5, 0.3, 0.2)
prioris <- gerate_priori_simplex_rec(unique(train$target), 10)
alpha <- 0.5