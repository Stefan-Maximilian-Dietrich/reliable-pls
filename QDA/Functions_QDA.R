# Hilfsfunktion für Dateinamen
.mk_title <- function(base, exp, method, i){
  paste0(base, exp$data, "_L_", exp$L, "_U_", exp$U,
         "_alp_", exp$alp, "_", exp$prio_t, "_", exp$prio_r,
         "/", method, "/", "ID_", i)
}

sampler_NB_up <- function(n_labled, n_unlabled, data, formula) {
  variables <- all.vars(formula) 
  target <- variables[1]
  data_used <- data[, variables]
  
  categories <- unique(data_used[, target])
  if(length(categories)*2  > n_labled) {
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
    
    witch_diff <- apply(indicator, 2, function(x) all(x))
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

multi_gamma <- function(a, d) {
  pi^(d * (d - 1) / 4) * prod(gamma(a + (1 - (1:d)) / 2))
}

marg_likeli_class <- function(X, mu0, kappa0, Lambda0, nu0) {
  n <- nrow(X)
  d <- ncol(X)
  x_bar <- colMeans(X)
  
  # Streumatrix S
  S <- t(X - matrix(x_bar, n, d, byrow = TRUE)) %*% (X - matrix(x_bar, n, d, byrow = TRUE))
  
  # Posterior-Parameter
  kappa_n <- kappa0 + n
  nu_n <- nu0 + n
  diff <- matrix(x_bar - mu0, ncol = 1)
  Lambda_n <- Lambda0 + S + ((kappa0 * n) / (kappa0 + n)) * (diff %*% t(diff))
  
  # Log-Marginal Likelihood nach Murphy Eq. (266)
  log_pi_term <- - (n * d / 2) * log(pi)
  log_det_term <- (nu0 / 2) * log(det(Lambda0)) - (nu_n / 2) * log(det(Lambda_n))
  log_kappa_term <- (d / 2) * (log(kappa0) - log(kappa_n))
  
  # Multivariate Gammafunktion: log(Γ_d(nu/2))
  log_multigamma <- function(a, p) {
    sum(lgamma(a + (1 - (1:p)) / 2))
  }
  
  log_gamma_term <- log_multigamma(nu_n / 2, d) - log_multigamma(nu0 / 2, d)
  
  # Summe aller Log-Terme
  log_marg_lik <- log_pi_term + log_gamma_term + log_det_term + log_kappa_term
  
  return(exp(log_marg_lik))
}

marg_likeli_class2 <- function(X, mu0, kappa0, Lambda0, nu0) {
  n <- nrow(X)
  d <- ncol(X)
  x_bar <- colMeans(X)
  
  S <- t(X - matrix(x_bar, n, d, byrow = TRUE)) %*% (X - matrix(x_bar, n, d, byrow = TRUE))
  kappa_n <- kappa0 + n
  nu_n <- nu0 + n
  Lambda_n <- Lambda0 + S + ((kappa0 * n) / (kappa0 + n)) * (matrix(x_bar - mu0, ncol = 1) %*% t(matrix(x_bar - mu0, ncol = 1)))
  
  part1 <- 1 / (pi^(n * d / 2))
  part2 <- multi_gamma(nu_n / 2, d) / multi_gamma(nu0 / 2, d)
  part3 <- det(Lambda0)^(nu0 / 2) / det(Lambda_n)^(nu_n / 2)
  part4 <- (kappa0 / kappa_n)^(d / 2)
  
  return(part1 * part2 * part3 * part4)
}

marg_likeli<- function(data_train , mu0, kappa0, Lambda0, nu0) {
  classes <- unique(data_train[["target"]])
  
  marg_lik <- 0
  for(class in classes) {
    X_df <-  data_train[data_train$target == class, ]
    X_df$target <- NULL
    X <- as.matrix(X_df)
    marg_lik = marg_lik + marg_likeli_class(X, mu0, kappa0, Lambda0, nu0) 
  }
  
  return(marg_lik)
}

marginal_likelihoods <- function(data_train, priors) {
  mag_Likelis <- lapply(seq_along(priors), function(i) {
    prior <- priors[[i]]
    marg_likeli(data_train, mu0 = prior$mu0, kappa0 = prior$kappa0,
                Lambda0 = prior$Lambda0, nu0 = prior$nu0)
  })
  return(mag_Likelis)
}

Posterior <- function(data_train, priors) {
  X_df <- data_train
  X_df$target <- NULL
  X <- as.matrix(X_df)
  y <- data_train[["target"]]
  classes <- unique(y)
  n_classes <- length(classes)
  
  results <- list()
  
  for (i in seq_along(priors)) {
    pr <- priors[[i]]
    mu0 <- pr$mu0
    kappa0 <- pr$kappa0
    Lambda0 <- pr$Lambda0
    nu0 <- pr$nu0
    
    for (cls in classes) {
      Xc <- X[y == cls, ]
      n <- nrow(Xc)
      d <- ncol(Xc)
      x_bar <- colMeans(Xc)
      S <- t(Xc - matrix(x_bar, n, d, byrow = TRUE)) %*% (Xc - matrix(x_bar, n, d, byrow = TRUE))
      
      # Updates
      kappa_n <- kappa0 + n
      nu_n <- nu0 + n
      mu_n <- (kappa0 * mu0 + n * x_bar) / kappa_n
      diff <- matrix(x_bar - mu0, ncol = 1)
      Lambda_n <- Lambda0 + S + (kappa0 * n / ( kappa0 + n)) * (diff %*% t(diff))
      
      results[[paste0("Prior_", i)]][[ cls]] <-list(
        Prior = paste0("Prior_", i),
        Class = cls,
        mu_n = mu_n,
        kappa_n = kappa_n,
        Lambda_n = Lambda_n,
        nu_n = nu_n
      )
    }
  }
  return(results)
}

Pseudo_Posterior_Predictive <- function(data_train,  data_point_pseudo, posterior) {
  class <-  data_point_pseudo[["target"]]
  posterior_class  <- posterior[[as.character(class)]]
  data_train_pseudo <- rbind(data_train,  data_point_pseudo)
  X_df <-  data_train_pseudo[data_train_pseudo$target == class, ]
  X_df$target <- NULL
  X <- as.matrix(X_df)
  PPP <- marg_likeli_class(X,  mu0 = posterior_class$mu_n, kappa0 = posterior_class$kappa_n, Lambda0 = posterior_class$Lambda_n, nu0 = posterior_class$nu_n)
  return(PPP)
}

PPP_matrix <- function(data_train, data_pseudo, posteriors) {
  ppp_m <- matrix(NA, nrow = nrow(data_pseudo), ncol = length(posteriors))
  for(i in 1:length(posteriors)) {
    posterior <- posteriors[[i]]
    for(j in 1:nrow(data_pseudo)) {
      data_point_pseudo <- data_pseudo[j, ]
      ppp_m[j,i] <-Pseudo_Posterior_Predictive(data_train,  data_point_pseudo, posterior)
    }
  }
  return(ppp_m)
}

qda_predict_proba <- function(data_train, data_new, eps = 1e-0) {
  # Training
  y <- data_train$target
  X <- as.matrix(subset(data_train, select = -target))
  classes <- levels(y)
  
  if ("target" %in% colnames(data_new)) {
    data_new <- subset(data_new, select = -target)
  }
  Xnew <- as.matrix(data_new)
  
  # Parameter schätzen
  params <- lapply(classes, function(cls) {
    Xc <- X[y == cls, , drop = FALSE]
    mu <- colMeans(Xc)
    Sigma <- cov(Xc)
    pi_k <- nrow(Xc) / nrow(X)
    list(mu = mu, Sigma = Sigma, pi_k = pi_k)
  })
  names(params) <- classes
  
  # Diskriminantenfunktionen δ_k(x)
  discrim <- sapply(classes, function(cls) {
    mu <- params[[cls]]$mu
    Sigma <- params[[cls]]$Sigma
    pi_k <- params[[cls]]$pi_k
    
    # Ridge-Regularisierung für numerische Stabilität
    Sigma_reg <- Sigma + diag(eps, nrow(Sigma))
    
    # Cholesky-Zerlegung (stabil für pos. definite Matrizen)
    cholS <- chol(Sigma_reg)
    invS <- chol2inv(cholS)
    logdetS <- 2 * sum(log(diag(cholS)))  # stabiler log(det(Sigma))
    
    apply(Xnew, 1, function(x) {
      delta <- -0.5 * logdetS -
        0.5 * t(x - mu) %*% invS %*% (x - mu) +
        log(pi_k)
      as.numeric(delta)
    })
  })
  
  if(is.null(nrow(discrim))) {
    discrim <- matrix(discrim, nrow = 1)
    
  } else {
    discrim <- as.matrix(discrim)
    
  }
  nrow(discrim)
  # Numerisch stabilisieren via Softmax
  discrim <- discrim - apply(discrim, 1, max)  # Trick gegen Overflow
  exp_disc <- exp(discrim)
  probs <- exp_disc / rowSums(exp_disc)
  
  colnames(probs) <- classes
  return(as.data.frame(probs))
}

qda_predict_class <- function(data_train, data_new) {
  probs <- qda_predict_proba(data_train, data_new)
  pred <- max.col(probs, ties.method = "first")
  new_target <- factor(colnames(probs)[pred], levels = levels(data_train$target))
  data_new$target <- new_target
  return(data_new)
}

test_confiusion <- function(data_train, data_test) {
  hard_preds <- qda_predict_class(data_train, data_test)
  true_labels <- data_test$target
  predicted_labels <- hard_preds$target
  confiusion <- caret::confusionMatrix(predicted_labels, true_labels)
  return(confiusion)
}

alpha_cut <- function(marg_prioris, alpha, priors) {
  marg_likelis <- unlist(marg_prioris)
  max_marg_likeli <- max(marg_likelis)
  cut_id <- marg_likelis >= alpha * max_marg_likeli
  cut_prioris <- priors[cut_id]
  return(cut_prioris)
}

generate_random_priors <- function(data, n_priors = 1000,  kappa_range = c(0.1, 2), scale_range = c(0.5, 2),  nu_extra = c(0, 5, 10)) {
  # Falls die Zielvariable "target" drin ist → entfernen
  if ("target" %in% colnames(data)) {
    data <- subset(data, select = -target)
  }
  
  X <- as.matrix(data)
  d <- ncol(X)
  mu_global <- colMeans(X)
  cov_global <- cov(X)
  
  priors <- list()
  
  for (i in 1:n_priors) {
    # µ0: globales Mittel + Zufallsrauschen
    mu0 <- mu_global + rnorm(d, sd = apply(X, 2, sd))
    
    # κ0: zufällig im Bereich
    kappa0 <- runif(1, min = kappa_range[1], max = kappa_range[2])
    
    # Λ0: skaliertes Cov oder isotrop
    if (runif(1) < 0.5) {
      c <- runif(1, min = scale_range[1], max = scale_range[2])
      Lambda0 <- c * cov_global
    } else {
      c <- runif(1, min = scale_range[1], max = scale_range[2])
      Lambda0 <- c * diag(d)
    }
    if(det(Lambda0) < 0) {
      c <- runif(1, min = scale_range[1], max = scale_range[2])
      Lambda0 <- c * diag(d)
    }
    # ν0: mindestens d+1, plus random offset
    nu0 <- sample(d + 1 + nu_extra, 1)

    priors[[i]] <- list(mu0 = mu0, kappa0 = kappa0, Lambda0 = Lambda0, nu0 = nu0)
  }
  
  return(priors)
}

######
row_has_one <- function(mat) {
  # Überprüfe für jede Zeile, ob mindestens eine 1 vorhanden ist
  result <- apply(mat, 1, function(x) any(x == 1))
  
  return(result)
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

e_admissible_creterion <- function(matrix) {
  indicator <- generate_indicator_matrix(matrix)
  bool_vec <- row_has_one(indicator)
  result <- which(bool_vec)
  return(result)
  
}


M_MaxiMin_creterion <- function(matrix) {
  a <- apply(matrix, 1, min)
  a_s <- which.max(a)[1]
  return(a_s)
}


M_MaxiMax_creterion <- function(matrix) {
  a <- apply(matrix, 1, max)
  a_s <- which.max(a)[1]
  return(a_s)
  
}

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
  
  if(is.null(return_vec)) {
    for(i in 1:n) {
      compare <- (1:n)[1:n != i]
      bool_vec <- NULL
      for(j in compare) {
        if(any(matrix[i, ] >= matrix[j, ])) {
          bool_vec <- c(bool_vec, TRUE)
        } else {
          bool_vec <- c(bool_vec, FALSE)
        }
      }
      if(all(bool_vec)) {
        return_vec <- c(return_vec,i)
        
      }
    }
    return_vec <- sample(return_vec, 1)
  }
  return(return_vec)
}

SSL_prob_criterion <- function(probabilitys) {
  max_per_row <- apply(probabilitys, 1, max)
  selsect <- which.max(max_per_row)
  return(selsect)
}

SSL_var_criterion <- function(probabilitys) {
  diff <- apply(probabilitys, 1, function(row) {
    sorted <- sort(row, decreasing = TRUE)
    sorted[1] - sorted[2]
  })
  selsect <- which.max(diff)
  return(selsect)
}

SSL_entropy_criterion <- function(probabilitys) {
  entropy <- apply(probabilitys, 1, function(p) {
    p <- p[p > 0]  # 0-Werte ausschließen, sonst log(0)
    -sum(p * log(p))
  })
  selsect <- which.min(entropy)
  
  return(selsect)
}

###################################
check_semaphor <- function() {
  load("/dss/dsshome1/03/di35lox/MASTER/SEMAPHOR")
  return(SEMAPHOR)
}

change_semaphor <- function(bool) {
  SEMAPHOR = bool
  save(SEMAPHOR, file = "/dss/dsshome1/03/di35lox/MASTER/SEMAPHOR")
}

wait <- function() {
  for(i in 1:15) {
    if(check_semaphor()) {
      change_semaphor(FALSE)
      break
    } else{
      Sys.sleep(1)
    } 
    if(i == 15) {
      stop("Zeitüberschreitung in der Semaphore")
      
    }
  }
}


######
experiment_to_adress <- function(exp) {
  adress <- paste0(exp$data, "_L_", exp$L, "_U_", exp$U, "_alp_", exp$alp, "_", exp$prio_t, "_", exp$prio_r)
  return(adress)
}

update_directory_strucutre <- function(){
  dir = "QDA"
  load("/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  for(i in 1:nrow(QDA)) {
    exp <- experiment_to_adress(QDA[i,])
    ordner <- paste0("/dss/dsshome1/03/di35lox/MASTER/results/",dir,"/", exp )
    if (!dir.exists(ordner)) {
      dir.create(ordner, recursive = TRUE)
    }
    methods <- c("SL","SSL_entropy", "SSL_variance" ,"SSL", "e_admissible", "maximal","M_MaxiMin", "M_MaxiMax")
    for(method in methods) {
      sub_ordner <-  paste0(ordner, "/", method)
      if (!dir.exists(sub_ordner)) {
        dir.create(sub_ordner, recursive = TRUE)
      }
    }
  }
}

data_loader <- function(dat) {
  source(paste(getwd(),"/QDA/data/in_use/", dat, ".R", sep = ""))
}
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

#####
make_Result_df <- function(experiment_path) {
  Methods_paths <- list.dirs(experiment_path, full.names = TRUE, recursive = FALSE)
  Methods <- basename(Methods_paths)
  experimet_result <- list()
  for(j in 1:length(Methods)) {
    run_adresses <- list.files(Methods_paths[j], full.names = TRUE)
    if(length(run_adresses) > 0) {
      all_accuracies <- list()
      for(k in 1:length(run_adresses)) {
        load(run_adresses[k])
        run <- get(Methods[j])
        accuracies <- sapply(run, function(x) x$overall["Accuracy"])
        all_accuracies[[k]] <- accuracies
      }
      accuracy_matrix <- do.call(cbind, all_accuracies)
      mean_result <- rowMeans(accuracy_matrix)
      experimet_result[Methods[j]] <- list(mean_result)
    }
    df <- do.call(rbind, lapply(names(experimet_result), function(nm) {
      data.frame(Method = nm, 
                 Index = 0:(length(experimet_result[[nm]]) - 1),   # Start bei 0
                 Accuracy = as.numeric(experimet_result[[nm]]))
    }))
  }
  return(df)
  
}

make_Result_matrix <- function(experiment_path) {
  df <- make_Result_df(experiment_path)
  
  
  accuracy_matrix <- pivot_wider(df,
                                 id_cols = Index,
                                 names_from = Method,
                                 values_from = Accuracy) %>%
    as.data.frame()
  
  # Index als rownames und aus den Daten entfernen
  rownames(accuracy_matrix) <- accuracy_matrix$Index
  accuracy_matrix$Index <- NULL
  
  # jetzt als Matrix
  mat <- as.matrix(accuracy_matrix)
  return(mat)
}

make_Result_Graph <- function(experiment_path) {
  name <- basename(experiment_path)
  
  df <- make_Result_df(experiment_path)
  G <- ggplot(df, aes(x = Index, y = Accuracy, color = Method)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    ggtitle(name)
  
  print(G)
  return(G)
}


get_experiment <- function(methods) {
  wait()
  adress <- paste0("/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  load(adress)
  QDA$overall <- NULL
  
  methos_vec <- unlist(methods)
  in_progress <- QDA$inProgress
  QDA$inProgress <- NULL
  to_do  <- !QDA[sapply(QDA, is.logical)]
  wanted <- to_do & matrix(methos_vec, nrow = nrow(to_do), ncol = ncol(to_do), byrow = TRUE)
  wanted_vac <- apply(wanted, 1, any)
  do_vec <- wanted_vac & !in_progress
  spezifications <- QDA[ !sapply(QDA, is.logical) ][do_vec, ][1,]
  to_dos <- wanted[do_vec, ][1,]
  row1 <- as.data.frame(
    c(as.list(spezifications),    # jeder Eintrag wird eigene Spalte
      list(inProgress = TRUE),
      as.list(to_dos)),
    check.names = FALSE
  )
  change_semaphor(TRUE)
  return(row1)
}

method_done <- function(experiemnt, method) {
  wait()
  load(file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  QDA[QDA$data ==  experiemnt$data & QDA$L == experiemnt$L & QDA$U ==  experiemnt$U & QDA$alpha ==  experiemnt$alp & QDA$prio_t == experiemnt$prio_t & QDA$prio_r == experiemnt$prio_r,method ] <- TRUE
  save(QDA, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  change_semaphor(TRUE)
}

change_progress <- function(experiemnt, bool) {
  wait()
  load(file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  QDA[QDA$data ==  experiemnt$data & QDA$L == experiemnt$L & QDA$U ==  experiemnt$U & QDA$alpha ==  experiemnt$alp & QDA$prio_t == experiemnt$prio_t & QDA$prio_r == experiemnt$prio_r,"inProgress" ] <- bool
  save(QDA, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")
  change_semaphor(TRUE)
}

