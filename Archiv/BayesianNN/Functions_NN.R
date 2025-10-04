combinations <- function(n, k) {
  if (k == 1) {
    return(matrix(n, nrow = 1))
  }
  
  result <- NULL
  for (i in 0:n) {
    rest <- combinations(n - i, k - 1)
    result <- rbind(result, cbind(i, rest))
  }
  return(result)
}

gerate_priori_simplex_rec <- function(levels, refinement){
  k <- length(levels)
  n <- refinement - k
  priori_matrix <- combinations(n, k)
  priori_matrix <- priori_matrix + 1
  priori_matrix <- priori_matrix/refinement
  colnames(priori_matrix) <- as.character(levels)
  
  return(as.data.frame(priori_matrix))
}

sampler_NN_up <- function(n_labled, n_unlabled, data, formula) {
  variables <- all.vars(formula) 
  target <- variables[1]
  data_used <- data[, variables]
  
  categories <- unique(data_used[, target])
  if(length(categories)  > n_labled) {
    stop("at least one instance cant be represented ")
  }
  
  train <- NULL
  for(cat in categories){
    data_temp <- data_used[data_used[,target]==cat, ]
    first <- data_temp[sample(1:nrow(data_temp), 1), , drop=FALSE]
    train <- rbind(train, first)

  }
  
  filterd <- data_used[!apply(data_used, 1, function(x) any(apply(train, 1, function(y) all(x == y)))), , drop=FALSE]
  n <- nrow(filterd)
  k <- nrow(train)
  
  train_idx <- sample(1:n, size = n_labled-k)  
  remaining_idx <- setdiff(1:n, train_idx)
  unlabeld_idx <- sample(remaining_idx, size = n_unlabled) 
  test_idx <- setdiff(remaining_idx, unlabeld_idx)  
  
  train_rest <- filterd[train_idx, ]
  train <- rbind(train, train_rest)
  unlabed <- filterd[unlabeld_idx,]
  test <- filterd[test_idx,]
  
  
  return(list(train, unlabed, test))
  
}

likelihood <- function(train, priori) { 
  # Modell
  nn <- nnet(train[,-c(1)], class.ind(train$target), size = 5, decay = 1e-3, maxit = 300, trace = FALSE, MaxNWts = 5000)
  
  # Vorhersage
  pred_prob<- predict(nn, train[,-c(1)], type = "raw")
  pred_w_prior <- sweep(as.matrix(pred_prob),  2,priori, "*")
  sums <- rowSums(pred_w_prior)
  posterior <- sweep(pred_w_prior, 1, sums, "/")
  true <- class.ind(train$target) 
  l <- prod(rowSums(true * posterior))

  ########
  
  
  return(l)
}

marginal_likelihoods <- function(train,  prioris) {
  marg_likelis <- NULL
  for(i in 1:nrow(prioris)) {
    marg_likelihood <- likelihood(train, as.numeric(prioris[i,]))
    marg_likelis <- c(marg_likelis, marg_likelihood)
  }
  
  marg_prioris <- cbind( marg_likelis, prioris)
  return(marg_prioris)
}

alpha_cut <- function(marg_prioris, alpha) {
  max <- max(marg_prioris$marg_likelis)
  cut_prioris <- marg_prioris[marg_prioris$marg_likelis >= alpha * max,]
  return(cut_prioris)
}

get_modell <- function(train) {
  nn <- nnet(train[,-c(1)], class.ind(train$target), size = 5, decay = 1e-3, maxit = 300, trace = FALSE, MaxNWts = 5000)
  return(nn)
}
