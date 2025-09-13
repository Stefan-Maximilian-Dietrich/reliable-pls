###### Sampler 
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
  
} # ToDo!



####### Marginal Likelihood
pack_theta <- function(W1, b1, W2, b2) {
  c(as.vector(W1), as.vector(b1), as.vector(W2), as.vector(b2))
}

unpack_theta <- function(theta) {
  stopifnot(length(theta) == theta_length)
  ofs <- 0
  W1 <- matrix(theta[(ofs + 1):(ofs + h*d)], nrow = h, ncol = d); ofs <- ofs + h*d
  b1 <- theta[(ofs + 1):(ofs + h)]; ofs <- ofs + h
  W2 <- matrix(theta[(ofs + 1):(ofs + K*h)], nrow = K, ncol = h); ofs <- ofs + K*h
  b2 <- theta[(ofs + 1):(ofs + K)]
  list(W1 = W1, b1 = b1, W2 = W2, b2 = b2)
}

forward_probs_theta <- function(X, theta) {
  pr <- unpack_theta(theta)
  n <- nrow(X)
  Hlin <- X %*% t(pr$W1) + matrix(pr$b1, n, h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(pr$W2) + matrix(pr$b2, n, K, byrow = TRUE)
  softmax_rows(Z)
}

loglik_theta <- function(theta, data_scaled) {
    X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
    y <- factor(data_scaled$Species, levels = classes)
    P <- forward_probs_theta(X, theta)        # n x K
    colnames(P) <- classes
    eps <- .Machine$double.eps
    P <- pmax(pmin(P, 1 - eps), eps)
    idx <- cbind(seq_len(nrow(X)), as.integer(y))
    sum(log(P[idx]))
}

log_prior_normal <- function(theta, mu, tau2) {
  p <- length(theta)
  r <- theta - mu
  -0.5 * (sum(r^2) / tau2) - 0.5 * p * (log(2*pi) + log(tau2))
}

neg_log_post <- function(theta, data_scaled, mu, tau2) {
  - (loglik_theta(theta, data_scaled) + log_prior_normal(theta, mu, tau2))
}

grad_neg_log_post <- function(theta, data_scaled, mu, tau2) {
  # Grad der negativen Loglik
  pr <- unpack_theta(theta)
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  y <- factor(data_scaled$Species, levels = classes)
  n <- nrow(X)
  
  # One-hot
  Y <- matrix(0, n, K)
  Y[cbind(seq_len(n), as.integer(y))] <- 1
  
  fw <- forward_full(X, pr$W1, pr$b1, pr$W2, pr$b2)
  P  <- fw$P; H <- fw$H
  
  # dL/dZ = P - Y (für NLL)
  dZ <- P - Y
  gW2 <- t(dZ) %*% H
  gb2 <- colSums(dZ)
  
  S  <- H * (1 - H)
  dH <- dZ %*% pr$W2 * S
  
  gW1 <- t(dH) %*% X
  gb1 <- colSums(dH)
  
  g_nll <- c(as.vector(gW1), as.vector(gb1), as.vector(gW2), as.vector(gb2))
  # + Prior-Anteil: (theta - mu)/tau2
  g_nll + (theta - mu) / tau2
}

get_marginal_likelihood <- function(prior_name, mu, tau2, theta_init, data_scaled_train, data_scaled_test = NULL, control = list(maxit = 1000, reltol = 1e-8)) {
  # MAP-Optimierung (BFGS)
  opt <- optim(
    par = theta_init,
    fn  = function(th) neg_log_post(th, data_scaled_train, mu, tau2),
    gr  = function(th) grad_neg_log_post(th, data_scaled_train, mu, tau2),
    method = "BFGS",
    control = control
  )
  
  theta_map <- opt$par
  conv      <- opt$convergence == 0
  
  # Hesse der negativen Log-Posterior am MAP
  H <- hessian(func = function(th) neg_log_post(th, data_scaled_train, mu, tau2), x = theta_map)
  
  # log|H| robust
  ld <- robust_logdet_pd(H)
  
  p <- length(theta_map)
  loglik_map  <- loglik_theta(theta_map, data_scaled_train)
  logprior_map<- log_prior_normal(theta_map, mu, tau2)
  log_joint   <- loglik_map + logprior_map
  log_evid    <- log_joint + 0.5 * p * log(2*pi) - 0.5 * ld$logdet
  
  # Optional: Test-Accuracy (MAP)
  acc_test <- NA_real_
  if (!is.null(data_scaled_test)) {
    predict_class_theta <- function(theta, data_scaled) {
      X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
      P <- forward_probs_theta(X, theta)
      factor(classes[max.col(P, ties.method = "first")], levels = classes)
    }
    pred <- predict_class_theta(theta_map, data_scaled_test)
    cm   <- table(Truth = data_scaled_test$Species, Pred = pred)
    acc_test <- sum(diag(cm)) / sum(cm)
  }
  
  list(
    name         = prior_name,
    tau2         = tau2,
    mu_norm      = sqrt(sum(mu^2)),
    map_converged= conv,
    loglik_map   = loglik_map,
    logprior_map = logprior_map,
    logdetH      = ld$logdet,
    jitter_used  = ld$jitter,
    log_evidence = log_evid,
    theta_map    = theta_map,
    acc_test     = acc_test
  )
}

marginal_likelihoods <- function(train_scaled,  priors) {
  marg_likelis <- NULL
  for(i in 1:length(priors)) {
    prior_ID <-  priors[[i]]$ID
    mu <- priors[[i]]$mu
    tau2 <- priors[[i]]$tau2
    theta_init <- rnorm(length(mu), sd = 0.3)
      
    marg_likelihood <- get_marginal_likelihood(prior_ID, mu, tau2, theta_init, train_scaled)
    
    marg_likelis <- c(marg_likelis, marg_likelihood$log_evidence)
  }
  
  IDs <- sapply(priors, function(x) x[[1]])
  
  marg_prioris <- cbind( marg_likelis, IDs)
  return(marg_prioris)
}

alpha_cut <- function(marg_prioris, alpha) {
  max <- max(marg_prioris[,1])
  cut_prioris <- marg_prioris[exp(marg_prioris[,1]) >= alpha * exp(max),]
  return(cut_prioris)
}



##### confusion

# Gradien Decend
sigmoid <- function(z) 1 / (1 + exp(-z))

softmax_rows <- function(Z) {
  # Z: n x K
  zmax <- apply(Z, 1, max)
  Zs   <- sweep(Z, 1, zmax, "-")
  EZ   <- exp(Zs)
  sweep(EZ, 1, rowSums(EZ), "/")
}

forward_full <- function(X, W1, b1, W2, b2) {
  # X: n x d
  n <- nrow(X)
  Hlin <- X %*% t(W1) + matrix(b1, n, h, byrow = TRUE)    # n x h
  H    <- sigmoid(Hlin)                                   # n x h
  Z    <- H %*% t(W2) + matrix(b2, n, K, byrow = TRUE)    # n x K
  P    <- softmax_rows(Z)                                  # n x K
  list(Hlin = Hlin, H = H, Z = Z, P = P)
}

gradient_decent <- function(train_scaled, lr = 0.05, epochs = 400, lambda = 1e-3 ) {
  d <- ncol(train_scaled) - 1
  Xtr <- as.matrix(train_scaled[, 1:d, drop = FALSE])
  Ytr <- one_hot(train_scaled$Species, classes)
  
  # Initialisierung
  W1 <- matrix(rnorm(h * d, sd = 0.3), nrow = h, ncol = d)
  b1 <- rnorm(h, sd = 0.3)
  W2 <- matrix(rnorm(K * h, sd = 0.3), nrow = K, ncol = h)
  b2 <- rnorm(K, sd = 0.3)
  
  n       <- nrow(Xtr)
  for (ep in seq_len(epochs)) {
    fw <- forward_full(Xtr, W1, b1, W2, b2)
    P  <- fw$P
    H  <- fw$H
    
    # Numerische Stabilität
    eps <- .Machine$double.eps
    P <- pmax(pmin(P, 1 - eps), eps)
    
    # Loss (sum) = -sum Y*log P + L2
    loss_ce <- -sum(Ytr * log(P))
    loss_reg <- (lambda/2) * (sum(W1^2) + sum(W2^2))
    loss <- loss_ce + loss_reg
    
    # Backpropagation
    # dZ = P - Y (n x K)
    dZ <- P - Ytr
    # Grad W2: K x h ; b2: K
    gW2 <- t(dZ) %*% H + lambda * W2
    gb2 <- colSums(dZ)
    
    # dH = dZ * W2  ⊙  sigmoid'(Hlin) = H*(1-H)
    dH <- dZ %*% W2 * (H * (1 - H))  # n x h
    
    # Grad W1: h x d ; b1: h
    gW1 <- t(dH) %*% Xtr + lambda * W1
    gb1 <- colSums(dH)
    
    # Parameter-Update (Gradient Descent)
    W2 <- W2 - lr * gW2
    b2 <- b2 - lr * gb2
    W1 <- W1 - lr * gW1
    b1 <- b1 - lr * gb1
    
    # Optional: einfache Lernratenabsenkung
    if (ep %% 150 == 0) lr <- lr * 0.5
    
    if (ep %% 50 == 0) {
      cat(sprintf("Epoch %3d | Loss: %.3f | CE: %.3f | Reg: %.3f\n", ep, loss, loss_ce, loss_reg))
    }
  }
  theta_hat <- pack_theta(W1, b1, W2, b2)
  
  return(theta_hat)
  
}
#

forward_probs_theta <- function(X, theta) {
  pr <- unpack_theta(theta)
  Hlin <- X %*% t(pr$W1) + matrix(pr$b1, nrow(X), h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(pr$W2) + matrix(pr$b2, nrow(X), K, byrow = TRUE)
  softmax_rows(Z)
}

predict_class_theta <- function(theta, data_scaled) {
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  P <- forward_probs_theta(X, theta)
  factor(classes[max.col(P, ties.method = "first")], levels = classes)
}

test_confiusion <- function(train_scaled, test_scaled) {
  theta_hat <- gradient_decent(train_scaled)
  predictions <- predict_class_theta(theta_hat, test_scaled)

  ground_truth <- as.factor(test_scaled$Species)
  confiusion <- caret::confusionMatrix(predictions, ground_truth)
  return(confiusion)
}


##### Predict 
predict_pseudo_labels <- function(train_scaled, unlabeled_scaled) {
  pseudolabeled_scaled <- unlabeled_scaled
  pseudolabeled_scaled$Species <- NULL
  
  theta_hat <- gradient_decent(train_scaled)
  predictions <- predict_class_theta(theta_hat, unlabeled_scaled)
  pseudolabeled_scaled$Species <- predictions
  return(pseudolabeled_scaled)
}

combine_lab_unlab <- function(train_scaled, pseudolabeled_scaled) {
  # Liste mit Länge = Anzahl Spalten von B
  result <- list()
  
  for (j in seq_len(nrow(pseudolabeled_scaled))) {
    result[[j]] <- rbind(train_scaled, pseudolabeled_scaled[ j,])
  }
  
  return(result)
}


####### PPP
log_prior_normal <- function(theta, mu, tau2) {
  p <- length(theta); r <- theta - mu
  -0.5 * (sum(r^2) / tau2) - 0.5 * p * (log(2*pi) + log(tau2))
}

neg_log_joint_D <- function(theta, data_scaled_train, mu, tau2) {
  # −[ log p(D|θ) + log π(θ) ]
  -(loglik_data(theta, data_scaled_train) + log_prior_normal(theta, mu, tau2))
}

grad_neg_log_joint_D <- function(theta, data_scaled_train, mu, tau2) {
  # ∇(−log p(D|θ)) + ∇(−log π(θ)) = grad_nll + (θ−μ)/τ²
  grad_neg_loglik_data(theta, data_scaled_train) + (theta - mu)/tau2
}

laplace_evidence_D <- function(mu, tau2, theta_start, data_scaled_train, control = list(maxit = 1000, reltol = 1e-8)) {
  opt <- optim(
    par = theta_start,
    fn  = function(th) neg_log_joint_D(th, data_scaled_train, mu, tau2),
    gr  = function(th) grad_neg_log_joint_D(th, data_scaled_train, mu, tau2),
    method = "BFGS",
    control = control
  )
  theta_mapD <- opt$par
  H_D <- hessian(function(th) neg_log_joint_D(th, data_scaled_train, mu, tau2), theta_mapD)
  ld  <- robust_logdet_pd(H_D)
  log_joint_map <- loglik_data(theta_mapD, data_scaled_train) + log_prior_normal(theta_mapD, mu, tau2)
  log_evidence  <- log_joint_map + 0.5 * p_dim * log(2*pi) - 0.5 * ld$logdet
  list(theta_mapD = theta_mapD, log_evidence = log_evidence, logdetH = ld$logdet, jitter = ld$jitter,
       converged = (opt$convergence == 0))
}

# Grad der negativen \tilde{l} (für BFGS)
grad_neg_ltilde <- function(theta, data_scaled_train, x_vec, yhat_label) {
  df1 <- make_single_df(x_vec, yhat_label)
  2 * grad_neg_loglik_data(theta, data_scaled_train) + grad_neg_loglik_data(theta, df1)
}

ltilde <- function(theta, data_scaled_train, x_vec, yhat_label) {
  2 * loglik_data(theta, data_scaled_train) + loglik_pseudo(theta, x_vec, yhat_label)
}

ppp_laplace_single <- function(mu, tau2, x_vec, yhat_label, data_scaled_train, theta_start_for_tilde, control = list(maxit = 1000, reltol = 1e-8)) {
  # Optimum \tilde{\theta} = argmax \tilde{l} (ohne Prior, gemäß deiner Approximation)
  opt_tilde <- optim(
    par = theta_start_for_tilde,
    fn  = function(th) -ltilde(th, data_scaled_train, x_vec, yhat_label),         # negative \tilde{l}
    gr  = function(th)  grad_neg_ltilde(th, data_scaled_train, x_vec, yhat_label),# Grad der negativen \tilde{l}
    method = "BFGS",
    control = control
  )
  theta_tilde <- opt_tilde$par
  lt_val      <- ltilde(theta_tilde, data_scaled_train, x_vec, yhat_label)
  # Hesse der negativen \tilde{l} am Optimum (≈ observed Fisher des Integranden ohne Prior)
  H_tilde <- hessian(function(th) -ltilde(th, data_scaled_train, x_vec, yhat_label), theta_tilde)
  ld_tilde <- robust_logdet_pd(H_tilde)
  
  # Prior-Dichte am \tilde{\theta}
  log_prior_at <- log_prior_normal(theta_tilde, mu, tau2)
  
  # Rückgabe der Bausteine; Division durch p(D) erst im äußeren Ablauf (weil p(D) nur vom Prior abhängt)
  list(theta_tilde = theta_tilde,
       log_ltilde  = lt_val,
       log_prior_tilde = log_prior_at,
       logdetH_tilde = ld_tilde$logdet,
       jitter_tilde   = ld_tilde$jitter,
       converged = (opt_tilde$convergence == 0))
}

PPP_matrix <- function(priors, train_scaled, pseudolabeled_scaled) { #ToDo
  results <- list()
  predictors <- setdiff(names(pseudolabeled_scaled), "Species")
  pseudo_points <- apply(pseudolabeled_scaled, 1, function(row) {
    list(
      x = as.numeric(row[predictors]),
      yhat = row[["Species"]]
    )
  })
  for (pr in priors) {
    # Evidence p(D) für diesen Prior (einmalig)
    evD <- laplace_evidence_D(mu = pr$mu, tau2 = pr$tau2, theta_start = pr$mu, data_scaled_train = train_scaled)
    log_pD <- evD$log_evidence
    print(pr$ID)
    i = 0
    for (ps in pseudo_points) {
      i = i + 1 
      # \tilde{\theta} für diesen (x_i, yhat_i); start sinnvoll (z.B. theta_pre_map)
      print(ps$x)
      theta_pre_map <- rnorm(length(pr$mu), sd = 0.3)
      
      ppp_parts <- ppp_laplace_single(
        mu = pr$mu, tau2 = pr$tau2,
        x_vec = ps$x, yhat_label = ps$yhat,
        data_scaled_train = train_scaled,
        theta_start_for_tilde = theta_pre_map
      )
      
      # Log-PPP gemäß deiner Approximation
      log_PPP <- ppp_parts$log_ltilde + ppp_parts$log_prior_tilde - log_pD + 0.5 * p_dim * log(2*pi) - 0.5 * ppp_parts$logdetH_tilde
      
      results[[length(results)+1]] <- data.frame(
        priorID      = pr$ID,
        tau2       = pr$tau2,
        psID = i,
        yhat       = ps$yhat,
        conv_tilde = ppp_parts$converged,
        conv_pD    = evD$converged,
        log_pD     = log_pD,
        log_ltilde = ppp_parts$log_ltilde,
        log_prior_at_tilde = ppp_parts$log_prior_tilde,
        logdetH_tilde = ppp_parts$logdetH_tilde,
        jitter_tilde = ifelse(is.na(ppp_parts$jitter_tilde), NA, ppp_parts$jitter_tilde),
        log_PPP    = log_PPP
      )
    }
  }
  
  
  results_df <- do.call(rbind, lapply(results, as.data.frame))
  return(results_df)
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

#########

