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
  features <- train[, !(names(train) %in% target), drop = FALSE]
  
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
    test_idx <- setdiff(remaining_idx, unlabeld_idx)  
    
    train <- data_used[train_idx, ]
    unlabed <- data_used[unlabeld_idx,]
    test <- data_used[test_idx,]
    
    again <- check_untrainability(train, data_used ,target)
    
  }
  
  return(list(train, unlabed, test))
  
}

sampler_NB_up <- function(n_labled, n_unlabled, data, formula) {
  variables <- all.vars(formula) 
  target <- variables[1]
  data_used <- data[, variables]
  
  categories <- unique(data_used[, target])
  if(length(categories)*2 > n_labled) {
    stop("Labeld date less than reqiert to fit a GNB")
  }
  
  train <- NULL
  for(cat in categories){
    data_temp <- data_used[data_used[,target]==cat, ]
    first <- data_temp[sample(1:nrow(data_temp), 1), , drop=FALSE]
    train <- rbind(train, first)
    col_to_check <- 2:ncol(data_temp)
    #x <- data_temp[1,]
    indicator <- !apply(data_temp, 1, function(x) {as.numeric(first[col_to_check]) == as.numeric(x)[-1]})
    
    witch_diff <- apply(indicator, 2, all)
    if(!any(witch_diff)) {
      stop(paste("Data set is not viable for GAUSIAN Naive Bayes. Problem in", cat))
    }
    
    data_temp2 <- data_temp[witch_diff, ]
    second <- data_temp2[sample(1:nrow(data_temp2), 1), , drop=FALSE]
    train <- rbind(train, second)
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
  model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(priori))
  posterior_probs <- predict(model, newdata = as.matrix(train[, -1]), type = "prob")
  
  
  
  ####
  vec_posterior_probs <- unlist(as.vector(posterior_probs))
  adress_True <- nrow(train) * (as.numeric(train$target) -1 )+ 1:nrow(train) 
  true_probs <- vec_posterior_probs[adress_True]
  false_probs <- vec_posterior_probs[-adress_True]
  
  
  false_likely <- sum(log1p(-false_probs))
  true_likely <- sum(log(true_probs))
  
  marg_likelihood <- true_likely +false_likely
  
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
  cut_prioris <- marg_prioris[marg_prioris$marg_likelis >= (1+alpha) * max,]
  return(cut_prioris)
}

beta_cut <- function(marg_prioris, beta) { # beste prozenz der alphas 
  quant <- quantile(marg_prioris$marg_likelis, beta)
  cut_prioris <- marg_prioris[marg_prioris$marg_likelis >= quant,]
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

decison_matrix_2 <- function(cut_priori, pseudo_data) {
  cut_priori_r<- cut_priori[, -1]
  
  k <- length(pseudo_data)
  l <- nrow(cut_priori_r)
  matrix <- matrix(NA, nrow = k, ncol = l)
  
  for(i in 1:k) {
    train <- pseudo_data[[i]][[1]]
    for(j in 1:l) {
      priori <- as.numeric(cut_priori_r[j, ])
      new <-  likelihood(train,  priori)
      matrix[i,j] <- new
    }
  }
  
  
  
  #########
  
  return(matrix)
  
  
  #######
}

decison_matrix_3 <- function(cut_priori, pseudo_data) {
  
  #########
  
  priori_list <- cut_priori[, -1]
  data_list <- lapply(pseudo_data, function(x) x[[1]])
  result <- lapply(data_list, function(x) {
    apply(priori_list, 1, function(y) likelihood(x,  y))
  })
  matrix <- do.call(rbind, result)
  return(matrix)
  
  
  #######
}

likelihood_2 <- function(posterior_probs, train) { 
  vec_posterior_probs <- unlist(as.vector(posterior_probs))
  adress_True <- nrow(train) * (as.numeric(train$target) -1 )+ 1:nrow(train) 
  true_probs <- vec_posterior_probs[adress_True]
  false_probs <- vec_posterior_probs[-adress_True]
  
  
  false_likely <- sum(log1p(-false_probs))
  true_likely <- sum(log(true_probs))
  
  marg_likelihood <- true_likely +false_likely
  return(marg_likelihood)
}

manual_predict_2 <- function(means, sds, priors, new_data) {
  
  # Klassen (Spaltennamen der Mittelwertmatrix)
  classes <- colnames(means)
  
  # Ergebnis für jede neue Beobachtung (jede Zeile in new_data)
  result <- apply(new_data, 1, function(x) {
    
    # Berechne die Posterior-Wahrscheinlichkeit für jede Klasse
    posterior_probs <- sapply(classes, function(class) {
      
      # Berechne die Wahrscheinlichkeitsdichte jedes Merkmals in dieser Klasse
      feature_probs <- sapply(1:length(x), function(i) {
        # Mittelwert und Standardabweichung für das Merkmal i und Klasse
        mu <- means[i, class]
        sigma <- sds[i, class]
        
        # Berechne die Dichte der Normalverteilung für jedes Merkmal
        dnorm(x[i], mean = mu, sd = sigma)
      })
      
      # Multipliziere die Dichten und berücksichtige die Prior-Wahrscheinlichkeit der Klasse
      prior_prob <- priors[class]
      likelihood <- prod(feature_probs)
      posterior_prob <- prior_prob * likelihood
      return(posterior_prob)
    })
    
    # Normalisiere die Posterior-Wahrscheinlichkeiten, damit ihre Summe 1 ergibt
    total_prob <- sum(posterior_probs)
    posterior_probs_normalized <- posterior_probs / total_prob
    return(posterior_probs_normalized)
  })
  
  # Gebe die normalisierten Posterior-Wahrscheinlichkeiten zurück
  return(t(result))
}

manual_predict <- function(means, sds, priors, new_data) {
  
  # Klassen (Spaltennamen der Mittelwertmatrix)
  classes <- colnames(means)
  
  # Ergebnis für jede neue Beobachtung (jede Zeile in new_data)
  result <- apply(new_data, 1, function(x) {
    
    # Berechne die Posterior-Wahrscheinlichkeit für jede Klasse
    posterior_probs <- sapply(classes, function(class) {
      
      # Berechne die Wahrscheinlichkeitsdichte jedes Merkmals in dieser Klasse
      
      # Mittelwert und Standardabweichung für das Merkmal i und Klasse
      mu <- means[, class]
      sigma <- sds[, class]
      
      # Berechne die Dichte der Normalverteilung für jedes Merkmal
      
      feature_probs <- dnorm(x, mean = mu, sd = sigma)
      
      
      # Multipliziere die Dichten und berücksichtige die Prior-Wahrscheinlichkeit der Klasse
      prior_prob <- priors[class]
      likelihood <- prod(feature_probs)
      posterior_prob <- prior_prob * likelihood
      return(posterior_prob)
    })
    
    # Normalisiere die Posterior-Wahrscheinlichkeiten, damit ihre Summe 1 ergibt
    total_prob <- sum(posterior_probs)
    posterior_probs_normalized <- posterior_probs / total_prob
    return(posterior_probs_normalized)
  })
  
  # Gebe die normalisierten Posterior-Wahrscheinlichkeiten zurück
  return(t(result))
}

decison_matrix_4 <- function(cut_priori, pseudo_data) {
  priori_list <- cut_priori[, -1]
  data_list <- lapply(pseudo_data, function(x) x[[1]])
  raw_GNB <- lapply(data_list, function(x) {gaussian_naive_bayes(x = as.matrix(x[, -1]), y = x$target)})
  
  matrix <- NULL
  for(i in 1:length(raw_GNB)) {
    list_data_modell <- apply(priori_list,1, function(x) {raw_GNB[[i]]$prior <- x  
    return(raw_GNB[[i]])} )
    
    prob <- lapply(list_data_modell, function(x){predict(x, newdata = as.matrix(data_list[[i]][, -1]), type = "prob")})
    
    matrix <- rbind(matrix, unlist(lapply(prob, function(x){likelihood_2(x, data_list[[i]])})))
  }
  
  return(matrix)
  #######
} # Aktuell schnelste 

decison_matrix<- function(cut_priori, pseudo_data) {
  priori_list <- cut_priori[, -1]
  data_list <- lapply(pseudo_data, function(x) x[[1]])
  raw_GNB <- lapply(data_list, function(x) {gaussian_naive_bayes(x = as.matrix(x[, -1]), y = x$target)})
  
  matrix <- NULL
  
  for(i in 1:length(raw_GNB)) {
    
    list_data_modell <- apply(priori_list,1, function(x) {raw_GNB[[i]]$prior <- x  
    return(raw_GNB[[i]])} )
    
    prob <- lapply(list_data_modell, function(x){predict(x, newdata = as.matrix(data_list[[i]][, -1]), type = "prob")})
    #prob <- lapply(list_data_modell, function(x){manual_predict(means =  t(x$params$mu), sds = t(x$params$sd), priors = x$prior, new_data = as.matrix(data_list[[i]][, -1]))})
    # für viele große datensätze 
    matrix <- rbind(matrix, unlist(lapply(prob, function(x){likelihood_2(x, data_list[[i]])})))
    
  }
  
  return(matrix)
  #######
} # Aktuell schnelste 

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

# Funktion zur Bestimmung der "maximalen" Objekte
maximalitaetskriterium <- function(matrix) {
  n <- nrow(matrix)
  return_vec <- NULL
  for(i in 1:n) {
    compare <- (1:n)[1:n != i]
    bool_vec <- NULL
    for(j in compare) {
      if(any(matrix[i, ] > matrix[j, ])) {
        bool_vec <- c(bool_vec, TRUE)
      } else {
        bool_vec <- c(bool_vec, FALSE)
      }
    }
    if(all(bool_vec)) {
      return_vec <- c(return_vec,i)
      
    }
  }
  return(return_vec)
}

test_confiusion <- function(priori, train, test) {
  
  if(is.null(priori)) {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train[, 1]))
    
  } else {
    model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(priori))
  }
  
  predictions <- predict(model, newdata = as.matrix(test[, -1]), type = "class")
  ground_truth <- test$target
  confiusion <- caret::confusionMatrix(predictions, ground_truth)
  return(confiusion)
}

predict_pseudo_labled_data <- function(best_modell, unlabeld) {
  X <- unlabeld[, -1]
  pseudo_y <-as.factor(predict(best_modell, newdata = as.matrix(X), type = "class"))
  pseudo_data_point <- cbind(target = pseudo_y, tibble(X))
  return(pseudo_data_point)
}

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
########################################################################
minimum_viable <- function(data, target) {
  categories <- unique(data[, target])
  min_vaib <- c(n_labled =length(categories)*2, n_unlabled = 0)
  return(min_vaib)
} 

generate_formula <- function(data, target) {
  predictors <- setdiff(names(data), target)  # Alle Spalten außer der Zielvariable
  formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
  return(as.formula(formula_str))  # Formel als R-Objekt zurückgeben
}

data_loader <- function(data_name) {
  source(paste(getwd(),"/NaiveBayes/data_NB/", data_name, ".R", sep = ""))
}


#### moitoring 
duration_function <- function(time_a, time_b) {
  duration <- as.numeric(difftime(time_b, time_a, units = "secs"))
  
  # Intelligente Formatierung
  if (duration < 60) {
    out <- sprintf("DONE in %.2f seconds", duration)
  } else if (duration < 3600) {
    minutes <- duration / 60
    out <- sprintf("DONE in %.2f minutes", minutes)
  } else if (duration < 86400) {
    hours <- duration / 3600
    out <- sprintf("DONE in %.2f hours", hours)
  } else {
    days <- duration / 86400
    out <- sprintf("DONE in %.2f days", days)
  }
  
  print(out)
}

Graphic_on_the_fly <- function(path) {

  
  load(path)
  mathods <- unique(names(gamma))
  
  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte <- lapply(matrizen, colMeans)
  
  # Umwandlung in ein Dataframe für ggplot
  df <- do.call(rbind, lapply(names(spalten_mittelwerte), function(name) {
    data.frame(Name = name, X = 1:length(spalten_mittelwerte[[name]]), 
               Mittelwert = spalten_mittelwerte[[name]])
  }))
  df$X <- df$X-1
  
  bereinigter_string <- sub(".*/", "", path)
  
  
  titel_d <- titel_data(path)
  titel_l <- extrahiere_zahl_L(path)
  titel_u <- extrahiere_zahl_U(path)
  plot_object <- ggplot(df, aes(x = X, y = Mittelwert, color = Name, group = Name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "unlabed Data", y = "test accuracy", title = paste0(titel_d, ": labeled = ", titel_l, ", unlabeled = ", titel_u))  
  
  print(plot_object)
  
  

}

#### Analyse
titel_data <- function(path) {
  text <- sub(".*/", "", path)
  
  sapply(text, function(x) {
    # 1. Kürzen ab "_L_"
    gekuerzt <- sub("_L_.*", "", x)
    
    # 2. Alles klein, dann Wortweise großschreiben (nach Leerzeichen)
    worte <- strsplit(tolower(gekuerzt), " ")[[1]]
    worte_cap <- paste0(toupper(substring(worte, 1, 1)), substring(worte, 2))
    zusammengesetzt <- paste(worte_cap, collapse = " ")
    
    # 3. Leerzeichen einfügen, wenn Kleinbuchstabe direkt vor Großbuchstabe kommt (z. B. "iA" → "i A")
    mit_abstand <- gsub("([a-z])([A-Z])", "\\1 \\2", zusammengesetzt)
    
    return(mit_abstand)
  })
}

extrahiere_zahl_L <- function(text) {
  regmatches(text, regexpr("(?<=_L_)[0-9]+", text, perl = TRUE))
}

extrahiere_zahl_U <- function(text) {
  regmatches(text, regexpr("(?<=_U_)[0-9]+", text, perl = TRUE))
}

make_all_Graphics <- function(online) {
  if(online) {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
    
  } else {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB_PC", sep="")
  }
  
  files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
  
  ###
  ###
  for(path in files) {
    print(path)
    load(path)
    mathods <- unique(names(gamma))
    
    gruppen <- split(gamma, mathods)
    matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
    spalten_mittelwerte <- lapply(matrizen, colMeans)
    
    # Umwandlung in ein Dataframe für ggplot
    df <- do.call(rbind, lapply(names(spalten_mittelwerte), function(name) {
      data.frame(Name = name, X = 1:length(spalten_mittelwerte[[name]]), 
                 Mittelwert = spalten_mittelwerte[[name]])
    }))
    df$X <- df$X-1
    
    bereinigter_string <- sub(".*/", "", path)
    
    
    titel_d <- titel_data(path)
    titel_l <- extrahiere_zahl_L(path)
    titel_u <- extrahiere_zahl_U(path)
    plot_object <- ggplot(df, aes(x = X, y = Mittelwert, color = Name, group = Name)) +
      theme(legend.position = "none") +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "unlabed Data", y = "test accuracy", title = paste0(titel_d, ": labeled = ", titel_l, ", unlabeled = ", titel_u))  
    
    ggsave(paste("/Users/Stefan/Desktop/Studium/Publikation/Experimetn_Grafiken/", bereinigter_string, ".png", sep = ""), width = 20, height = 20, units = "cm", dpi = 300,plot = plot_object)
    
  }
  
}

show_all_Results <- function(online) {
  if(online) {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
    
  } else {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB_PC", sep="")
  }
  files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
  improvment_matrix <- NULL
  for(path in files) { # anteil der fehler im vergleich zu SL 0 = alle fehler beseitigt 1= genua so viele fehler 2= doppelt so vile fehler 
    print(path)
    load(path)
    mathods <- unique(names(gamma))
    
    gruppen <- split(gamma, mathods)
    matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
    spalten_mittelwerte <- lapply(matrizen, colMeans)
    
    ead_end <- spalten_mittelwerte$e_admissible[length(spalten_mittelwerte$e_admissible)]
    erow_ref_point <- 1- spalten_mittelwerte$SL[1]
    errow_quote <- lapply(spalten_mittelwerte, function(x) 1- x[length(x)])
    improvement <- lapply(errow_quote, function(x)  x/erow_ref_point)
    ratio <- unlist(improvement)
    
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("[0-9]+(?:\\.[0-9]+)?", path))))
    names(numbers) <- c("L", "U", "alp")
    bereinigter_string <- sub(".*/", "", path)
    vec <- c(numbers, e_ad_end = ead_end, ratio)
    prefix <- sub("_.*", "", bereinigter_string)
    df <- data.frame(data = prefix, as.list(vec))
    improvment_matrix <- rbind(improvment_matrix, df)
    
    
  }
  
  View(improvment_matrix[order(improvment_matrix$data, improvment_matrix$L, improvment_matrix$U, improvment_matrix$alp),])
  
  
}

show_Summary <- function(online) {
  if(online) {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
    
  } else {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB_PC", sep="")
  }

  files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
  improvment_matrix <- NULL
  for(path in files) { # anteil der fehler im vergleich zu SL 0 = alle fehler beseitigt 1= genua so viele fehler 2= doppelt so vile fehler 
    print(path)
    load(path)
    mathods <- unique(names(gamma))
    
    gruppen <- split(gamma, mathods)
    matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
    spalten_mittelwerte <- lapply(matrizen, colMeans)
    
    ead_end <- spalten_mittelwerte$e_admissible[length(spalten_mittelwerte$e_admissible)]
    erow_ref_point <- 1- spalten_mittelwerte$SL[1]
    errow_quote <- lapply(spalten_mittelwerte, function(x) 1- x[length(x)])
    improvement <- lapply(errow_quote, function(x)  x/erow_ref_point)
    ratio <- unlist(improvement)
    
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("[0-9]+(?:\\.[0-9]+)?", path))))
    names(numbers) <- c("L", "U", "alp")
    bereinigter_string <- sub(".*/", "", path)
    vec <- c(numbers, e_ad_end = ead_end, ratio)
    prefix <- sub("_.*", "", bereinigter_string)
    df <- data.frame(data = prefix, as.list(vec))
    improvment_matrix <- rbind(improvment_matrix, df)
    
    
  }
  
  imp <- improvment_matrix[improvment_matrix$U > 2*improvment_matrix$L  ,]
  names_data_a <- unique(imp$data)
  summary_table <- NULL
  for(t in names_data_a){
    data_loader(t)
    p <- length(all.vars(formula)) - 1
    k <- length(unique(data$target))
    anzahl <- nrow(imp[imp$data == t,])
    besserSL <- paste0(round(sum(imp[imp$data == t,]$e_admissible < 1)/anzahl * 100), "%")
    besserSSL <- paste0(round(sum(imp[imp$data == t,]$e_admissible < imp[imp$data == t,]$SSL )/anzahl * 100), "%")
    row <- c(Datensatz = t, Variblen = p, Kategorien = k,  Anzahl_Experimente = anzahl, besser_als_SL = besserSL, besser_als_SSL = besserSSL)
    summary_table <- rbind(summary_table, row)
  }
  
  summary_table_df <- as.data.frame(summary_table)
  row.names(summary_table_df) <- NULL
  View(summary_table_df)
}

all_experiments <- function(online){
  if(online) {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
    
  } else {
    ground_path <-  paste(getwd(),"/NaiveBayes/results_NB_PC", sep="")
  }
  files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
  improvment_matrix <- NULL
  for(path in files) { # anteil der fehler im vergleich zu SL 0 = alle fehler beseitigt 1= genua so viele fehler 2= doppelt so vile fehler 
    print(path)
    load(path)
    mathods <- unique(names(gamma))
    
    gruppen <- split(gamma, mathods)
    matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
    spalten_mittelwerte <- lapply(matrizen, colMeans)
    
    ead_end <- spalten_mittelwerte$e_admissible[length(spalten_mittelwerte$e_admissible)]
    erow_ref_point <- 1- spalten_mittelwerte$SL[1]
    errow_quote <- lapply(spalten_mittelwerte, function(x) 1- x[length(x)])
    improvement <- lapply(errow_quote, function(x)  x/erow_ref_point)
    ratio <- unlist(improvement)
    
    numbers <- as.numeric(unlist(regmatches(path, gregexpr("[0-9]+(?:\\.[0-9]+)?", path))))
    names(numbers) <- c("L", "U", "alp")
    bereinigter_string <- sub(".*/", "", path)
    vec <- c(numbers, e_ad_end = ead_end, ratio)
    prefix <- sub("_.*", "", bereinigter_string)
    df <- data.frame(data = prefix, as.list(vec))
    improvment_matrix <- rbind(improvment_matrix, df)
    
    
  }
  
  return(improvment_matrix[,c(1,2,3,4)])
  
  
}
