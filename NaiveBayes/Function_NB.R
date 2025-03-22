check_untrainability <- function(train, data_used, target) {
  # Prüfen, ob jede Kategorie im Trainingsdatensatz vertreten ist
  category_table <- table(data_used[[target]])
  category_table_train <- table(train[[target]])
  
  if (!(length(category_table) == length(category_table_train))) {
    return(TRUE)
  }
  
  # Prüfen ob jede spalte mindesten 2 hat 
  if (!all(category_table_train > 1)) {
    return(TRUE)
  }
  

  # Spaltenweise prüfen, ob jede Kategorie mindestens zwei unterschiedliche Werte hat
  valid_features <- sapply(split(features, train[[target]]), function(sub_df) {
    apply(sub_df, 2, function(x) length(unique(x)) >= 2)
  })
  
  if(!all(valid_features)) {
    return(TRUE)
  }
  
  return(FALSE)

}

sampler_NB <- function(n_labled, n_unlabled, data, formula) {
  variables <- all.vars(formula) 
  target <- variables[1]
  data_used <- data[, variables]
  
  n <- nrow(data_used)
  
  again <- TRUE
  
  while(again) {
    train_idx <- sample(1:n, size = n_labled)  
    remaining_idx <- setdiff(1:n, train_idx)
    unlabeld_idx <- sample(remaining_idx, size = n_unlabled) 
    test_idx <- setdiff(remaining_idx, unlabeld_idx)  # 10% Test
    
    train <- data_used[train_idx, ]
    unlabed <- data_used[unlabeld_idx,]
    test <- data_used[test_idx,]
    
    
    again <- check_untrainability(train, data_used ,target)
    
  }
  
  return(list(train, unlabed, test))
  
}

likelihood <- function(train, priori) {   
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(priori))
  posterior_probs <- predict(model, newdata = as.matrix(train[, -1]), type = "prob")
  real_probs <- posterior_probs[cbind(1:nrow(train), train[, 1])]
  marg_likelihood <-  exp(sum(log(real_probs))) ### falls so beleibt umbenennen 
  return(marg_likelihood)
}

generate_priori_simplex <- function(categories, step = 0.1) {
  dims <- length(categories)
  
  # Erzeuge für jede Dimension eine Sequenz von 0 bis 1 mit der gegebenen Schrittweite
  seq_list <- replicate(dims, seq(0+step, 1-step, by = step), simplify = FALSE)
  
  # Erzeuge das vollständige Gitter
  grid <- do.call(expand.grid, seq_list)
  
  
  # Behalte nur die Zeilen, bei denen die Summe der Koordinaten ca. 1 ergibt
  simplex <- grid[rowSums(grid) == 1.000, ]
  
  # Optional: Setze die Spaltennamen, z. B. X1, X2, ..., Xd
  colnames(simplex) <- categories
  
  return(simplex)
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
  cut_prioris <- marg_prioris[marg_prioris$marg_likelis >= alpha*max,]
  return(cut_prioris)
}

predict_best_model <- function(cut_prioris, train) { #tunig möglich über best prior
  cut_boolean <- cut_prioris$marg_likelis == max(cut_prioris$marg_likelis)
  true_indices <- which(cut_boolean)
  if(length(true_indices) > 1) {
    choosen_index <- sample(x = as.vector(true_indices), 1)
    choosen_vector <- rep(FALSE, times = length(cut_boolean))
    choosen_vector[choosen_index] <- TRUE
  } else {
    choosen_index <- true_indices
    choosen_vector <- rep(FALSE, times = length(cut_boolean))
    choosen_vector[choosen_index] <- TRUE
  }
  
  
  best_priori <- cut_prioris[choosen_vector,][,-1]
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(best_priori))
  return(model)
}

pseud_labeling <- function(best_modell, train, unlabeld) {
  X <- unlabeld[, -1]
  Y <- unlabeld[, 1]
  names <- names(train)
  result_list <- list()
  for(i in 1:nrow(X)) {
    pseudo_y <-as.factor(predict(best_modell, newdata = as.matrix(X[i,]), type = "class"))
    pseudo_data_point <- cbind(target = pseudo_y, tibble(X[i,]))
    combined_data <- rbind(train, pseudo_data_point) 
    l <- list (combined_data, c(pseudo_y, Y[i]))
    result_list[[i]] <- l ######
    
  }
  return(result_list)
}

decison_matrix <- function(cut_priori, pseudo_data) {
  cut_priori<- cut_priori[, -1]
  
  k <- length(pseudo_data)
  l <- nrow(cut_priori)
  matrix <- matrix(NA, nrow = k, ncol = l)
  
  for(i in 1:k) {
    train <- pseudo_data[[i]][[1]]
    for(j in 1:l) {
      priori <- as.numeric(cut_priori[j, ])
      new <-  likelihood(train,  priori)
      matrix[i,j] <- new
    }
  }
  return(matrix)
}

generate_indicator_matrix <- function(mat) {
  # Bestimme die Anzahl der Reihen und Spalten
  m <- nrow(mat)
  n <- ncol(mat)
  # Erstelle eine leere Matrix mit der gleichen Dimension, die mit 0 gefüllt ist
  result <- matrix(0, nrow = m, ncol = n)
  
  # Gehe Spalte für Spalte durch
  for (j in 1:n) {
    # Bestimme das größte Element in der j-ten Spalte
    max_value <- max(mat[, j])
    
    # Setze die Positionen des größten Werts in der entsprechenden Spalte auf 1
    for (i in 1:m) {
      if (mat[i, j] == max_value) {
        result[i, j] <- 1
        i <- m ################
      }
    }
  }
  
  return(result)
} #GPT

row_has_one <- function(mat) {
  # Überprüfe für jede Zeile, ob mindestens eine 1 vorhanden ist
  result <- apply(mat, 1, function(x) any(x == 1))
  
  return(result)
} #GPT

e_admissible_creterion <- function(matrix) {
  indicator <- generate_indicator_matrix(matrix)
  bool_vec <- row_has_one(indicator)
  result <- which(bool_vec)
  return(result)
  
}

test_confiusion <- function(priori, train, test) {
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(priori))
  predictions <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  ground_truth <- test$target
  confiusion <- caret::confusionMatrix(predictions, ground_truth)
  return(confiusion)
}
###############################################################################
