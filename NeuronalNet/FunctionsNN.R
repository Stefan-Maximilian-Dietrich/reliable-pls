###### Sampler 
sampleNN <- function(data, formula, n_labled, n_unlabled) {
  include_na = FALSE
  seed = NULL
  variables <- all.vars(formula) 
  x <- variables[1]
  df <- data[, variables]
  
  
  stopifnot(is.data.frame(df), x %in% names(df))
  
  if (!include_na) df <- df[!is.na(df[[x]]), , drop = FALSE]
  if (nrow(df) == 0L) stop("Keine Daten (nach NA-Filter).")
  if (!is.null(seed)) set.seed(seed)
  
  # --- Set 1: jede Kategorie mindestens 1x ---
  idx_one_each <- unlist(lapply(split(seq_len(nrow(df)), df[[x]]), function(ix) sample(ix, 1L)))
  
  rest_needed <- n_labled - length(idx_one_each)
  
  idx_rest1 <- if (rest_needed > 0L) {
    sample(setdiff(seq_len(nrow(df)), idx_one_each), rest_needed)
  } else integer(0)
  idx_set1 <- sample(c(idx_one_each, idx_rest1))
  
  # Rest nach Set 1
  remaining_after_1 <- setdiff(seq_len(nrow(df)), idx_set1)
  
  # --- Set 2: Zufall aus Rest ---
  if (n_unlabled < 0) stop("n_unlabled muss >= 0 sein.")
  if (n_unlabled > length(remaining_after_1)) {
    stop(sprintf("n_unlabled (%d) > verbleibende Zeilen nach Set 1 (%d).",
                 n_unlabled, length(remaining_after_1)))
  }
  idx_set2 <- if (n_unlabled > 0L) sample(remaining_after_1, n_unlabled) else integer(0)
  
  # --- Set 3: alle übrigen ---
  idx_set3 <- setdiff(remaining_after_1, idx_set2)
  
  set1 <- df[idx_set1, , drop = FALSE]
  set2 <- df[idx_set2, , drop = FALSE]
  set3 <- df[idx_set3, , drop = FALSE]
  
  # --------- Standardisierung ---------
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    # Gemeinsame Stats für Set1+Set2
    comb <- rbind(set1[num_cols], set2[num_cols])
    m12 <- vapply(comb, mean, numeric(1), na.rm = TRUE)
    s12 <- vapply(comb, sd,   numeric(1), na.rm = TRUE)
    s12[s12 == 0 | is.na(s12)] <- 1
    
    scale_fun <- function(d, m, s) {
      for (nm in names(m)) d[[nm]] <- (d[[nm]] - m[[nm]]) / s[[nm]]
      d
    }
    
    set1 <- scale_fun(set1, m12, s12)
    set2 <- scale_fun(set2, m12, s12)
    
    # Separate Stats für Set3
    m3 <- vapply(set3[num_cols], mean, numeric(1), na.rm = TRUE)
    s3 <- vapply(set3[num_cols], sd,   numeric(1), na.rm = TRUE)
    s3[s3 == 0 | is.na(s3)] <- 1
    set3 <- scale_fun(set3, m3, s3)
  }
  
  list(
    set1_cover = set1,
    set2_random = set2,
    set3_rest   = set3
  )
}

data_loader <- function(data_name) {
  source(paste(getwd(),"/NeuronalNet/data/in_use/", data_name, ".R", sep = ""))
}

#Gererate Prioris 
gerate_normal_priori <- function(n_param, refinement) {
  priors <- list()
  for(i in 1:refinement) {
    priors[[i]] <- list(ID = i,  mu = rnorm(n_param, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1))
  }
  return(priors)
}

####### Marginal Likelihood
pack_theta <- function(W1, b1, W2, b2 ) {
  c(as.vector(W1), as.vector(b1), as.vector(W2), as.vector(b2))
}

unpack_theta <- function(theta) {
  stopifnot(length(theta) == n_param)
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
  #classes <- c("setosa", "versicolor" ,   "virginica")
  
  X <- as.matrix(data_scaled[, 2:(d+1), drop = FALSE])
  y <- factor(data_scaled$target)
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

loglik_data <- function(theta, data_scaled) {
  #classes <- c("setosa", "versicolor" ,   "virginica")
  
  X <- as.matrix(data_scaled[, 2:(d+1), drop = FALSE])
  y <- factor(data_scaled$target)
  P <- forward_probs_theta(X, theta)
  colnames(P) <- classes
  eps <- .Machine$double.eps
  P <- pmax(pmin(P, 1 - eps), eps)
  idx <- cbind(seq_len(nrow(X)), as.integer(y))
  sum(log(P[idx]))
}

grad_neg_loglik_data <- function(theta, data_scaled) {
  pr <- unpack_theta(theta)
  X <- as.matrix(data_scaled[, 2:(d+1), drop = FALSE])
  y <- factor(data_scaled$target)
  n <- nrow(X)
  
  # One-hot
  Y <- matrix(0, n, K)
  Y[cbind(seq_len(n), as.integer(y))] <- 1
  
  fw <- forward_full(X, pr$W1, pr$b1, pr$W2, pr$b2)
  P  <- fw$P; H <- fw$H
  
  dZ <- P - Y
  gW2 <- t(dZ) %*% H
  gb2 <- colSums(dZ)
  
  S  <- H * (1 - H)
  dH <- dZ %*% pr$W2 * S
  
  gW1 <- t(dH) %*% X
  gb1 <- colSums(dH)
  
  c(as.vector(gW1), as.vector(gb1), as.vector(gW2), as.vector(gb2))
}

neg_log_post <- function(theta, data_scaled, mu, tau2) {
  - (loglik_theta(theta, data_scaled) + log_prior_normal(theta, mu, tau2))
}

grad_neg_log_post <- function(theta, data_scaled, mu, tau2) {
  # Grad der negativen Loglik
  pr <- unpack_theta(theta)
  X <- as.matrix(data_scaled[, 2:(d+1), drop = FALSE])
  y <- factor(data_scaled$target)
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

get_marginal_likelihood <- function(prior_name, mu, tau2, theta_init, train_scaled, data_scaled_test = NULL, control = list(maxit = 1000, reltol = 1e-8)) {
  # MAP-Optimierung (BFGS)
  #classes <- c("setosa", "versicolor" ,   "virginica")
  
  opt <- optim(
    par = theta_init,
    fn  = function(theta) neg_log_post(theta, train_scaled, mu, tau2),
    gr  = function(theta) grad_neg_log_post(theta, train_scaled, mu, tau2),
    method = "BFGS",
    control = control
  )
  
  theta_map <- opt$par
  conv      <- opt$convergence == 0
  
  # Hesse der negativen Log-Posterior am MAP
  H <- hessian(func = function(th) neg_log_post(th, train_scaled, mu, tau2), x = theta_map)
  
  # log|H| robust
  ld <- robust_logdet_pd(H)
  
  p <- length(theta_map)
  loglik_map  <- loglik_theta(theta_map, train_scaled)
  logprior_map<- log_prior_normal(theta_map, mu, tau2)
  log_joint   <- loglik_map + logprior_map
  log_evid    <- log_joint + 0.5 * p * log(2*pi) - 0.5 * ld$logdet
  
  # Optional: Test-Accuracy (MAP)
  acc_test <- NA_real_
  if (!is.null(data_scaled_test)) {
    predict_class_theta <- function(theta, data_scaled) {
      #classes <- c("setosa", "versicolor" ,   "virginica")
      
      X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
      P <- forward_probs_theta(X, theta)
      factor(classes[max.col(P, ties.method = "first")], levels = classes)
    }
    pred <- predict_class_theta(theta_map, data_scaled_test)
    cm   <- table(Truth = data_scaled_test$target, Pred = pred)
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

alpha_cut <- function(marg_prioris, alpha, priors) {
  max <- max(marg_prioris[,1])
  marg_prioris <- as.data.frame(marg_prioris)
  cut <- marg_prioris[exp(marg_prioris[,1]) >= alpha * exp(max),]$IDs 
  cut_prioris <- priors[as.numeric(cut)]
  return(cut_prioris)
}



##### confusion

# Gradien Decend
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

softmax_rows <- function(Z) {
  # Z: n x K
  zmax <- apply(Z, 1, max)
  Zs   <- sweep(Z, 1, zmax, "-")
  EZ   <- exp(Zs)
  sweep(EZ, 1, rowSums(EZ), "/")
}

forward_full <- function(Xtr, W1, b1, W2, b2) {
  # X: n x d
  n <- nrow(Xtr)
  Hlin <- Xtr %*% t(W1) + matrix(b1, n, h, byrow = TRUE)    # n x h
  H    <- sigmoid(Hlin)                                   # n x h
  Z    <- H %*% t(W2) + matrix(b2, n, K, byrow = TRUE)    # n x K
  P    <- softmax_rows(Z)                                  # n x K
  list(Hlin = Hlin, H = H, Z = Z, P = P)
}

one_hot <- function(y, class_levels) {
  y <- factor(y, levels = class_levels)
  Y <- matrix(0, nrow = length(y), ncol = length(class_levels))
  Y[cbind(seq_along(y), as.integer(y))] <- 1
  colnames(Y) <- class_levels
  Y
}

gradient_decent <- function(train_scaled, lr = 0.05, epochs = 400, lambda = 1e-3 ) {
  #classes <- c("setosa", "versicolor" ,   "virginica")
  Xtr <- as.matrix(train_scaled[, 2:(1+d), drop = FALSE])
  Ytr <- one_hot(train_scaled$target, classes)
  
  #d <- ncol(train_scaled) - 1
  #h <- 5                  # Hidden Units (fix)
  #K <- length(classes) 
  
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
      # cat(sprintf("Epoch %3d | Loss: %.3f | CE: %.3f | Reg: %.3f\n", ep, loss, loss_ce, loss_reg))
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

predict_class_theta <- function(theta_hat, data_scaled) {
  #classes <- c("setosa", "versicolor", "virginica")
  
  X <- as.matrix(data_scaled[, c(2:(d+1)), drop = FALSE])
  P <- forward_probs_theta(X, theta_hat)
  colnames(P) <- classes
  predicted <- colnames(P)[apply(P, 1, which.max)]
  
}

test_confiusion <- function(train_scaled, test_scaled) {
  #classes <- c("setosa", "versicolor", "virginica")
  theta_hat <- gradient_decent(train_scaled)
  predictions <- predict_class_theta(theta_hat, test_scaled)
  
  ground_truth <- factor(test_scaled$target, levels = classes)
  predictions <- factor(predictions, levels = classes)
  
  confiusion <- caret::confusionMatrix(predictions, ground_truth)
  return(confiusion)
}

predict_class_theta_prob <- function(theta_hat, data_scaled) { 
  X <- as.matrix(data_scaled[, c(2:(d+1)), drop = FALSE])
  P <- forward_probs_theta(X, theta_hat)
  colnames(P) <-   levels(data_scaled$target)
  return(P)
}

predict_pseudo_labels_prob <- function(train_scaled, unlabeled_scaled) { 
  pseudolabeled_scaled <- unlabeled_scaled
  pseudolabeled_scaled$target <- NULL
  
  
  theta_hat <- gradient_decent(train_scaled)
  predictions <- predict_class_theta_prob(theta_hat, unlabeled_scaled)
  return(predictions)
}

##### Predict 
predict_pseudo_labels <- function(train_scaled, unlabeled_scaled) {
  pseudolabeled_scaled <- unlabeled_scaled
  pseudolabeled_scaled$target <- NA
  
  
  theta_hat <- gradient_decent(train_scaled)
  predictions <- predict_class_theta(theta_hat, unlabeled_scaled)
  pseudolabeled_scaled$target <- predictions
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

make_single_df <- function(x_vec, yhat_label, num_cols) {
  stopifnot(length(x_vec) == d)
  df <- as.data.frame(matrix(x_vec, nrow = 1))
  names(df) <- num_cols
  df$target <- factor(yhat_label)
  
  df <- df[, c(ncol(df), 1:(ncol(df)-1))]
  
  return(df)
}

grad_neg_log_joint_D <- function(theta, data_scaled_train, mu, tau2) {
  # ∇(−log p(D|θ)) + ∇(−log π(θ)) = grad_nll + (θ−μ)/τ²
  grad_neg_loglik_data(theta, data_scaled_train) + (theta - mu)/tau2
}

loglik_pseudo <- function(theta, x_vec, yhat_label,num_cols ) {
  df1 <- make_single_df(x_vec, yhat_label, num_cols)
  loglik_data(theta, df1)
}

robust_logdet_pd <- function(H) {
  jit_vals <- c(0, 1e-8, 1e-6, 1e-4, 1e-3)
  for (jit in jit_vals) {
    Hjit <- if (jit > 0) H + diag(jit, nrow(H)) else H
    ok <- TRUE
    ch <- tryCatch(chol(Hjit), error = function(e) { ok <<- FALSE; NULL })
    if (ok) return(list(logdet = 2*sum(log(diag(ch))), jitter = jit))
  }
  # Fallback über Eigenwerte (clamp)
  ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  ev_pos <- pmax(ev, 1e-12)
  list(logdet = sum(log(ev_pos)), jitter = NA_real_)
}

laplace_evidence_D <- function(mu, tau2, theta_start, data_scaled_train, control = list(maxit = 1000, reltol = 1e-8)) {
  opt <- optim(
    par = theta_start,
    fn  = function(theta) neg_log_joint_D(theta, data_scaled_train, mu, tau2),
    gr  = function(theta) grad_neg_log_joint_D(theta, data_scaled_train, mu, tau2),
    method = "BFGS",
    control = control
  )
  p_dim <-length(theta_start)
  theta_mapD <- opt$par
  H_D <- hessian(function(th) neg_log_joint_D(th, data_scaled_train, mu, tau2), theta_mapD)
  ld  <- robust_logdet_pd(H_D)
  log_joint_map <- loglik_data(theta_mapD, data_scaled_train) + log_prior_normal(theta_mapD, mu, tau2)
  log_evidence  <- log_joint_map + 0.5 * p_dim * log(2*pi) - 0.5 * ld$logdet
  list(theta_mapD = theta_mapD, log_evidence = log_evidence, logdetH = ld$logdet, jitter = ld$jitter,
       converged = (opt$convergence == 0))
}

# Grad der negativen \tilde{l} (für BFGS)
grad_neg_ltilde <- function(theta, data_scaled_train, x_vec, yhat_label, num_cols) {
  df1 <- make_single_df(x_vec, yhat_label, num_cols)
  2 * grad_neg_loglik_data(theta, data_scaled_train) + grad_neg_loglik_data(theta, df1)
}

ltilde <- function(theta, data_scaled_train, x_vec, yhat_label, num_cols) {
  2 * loglik_data(theta, data_scaled_train) + loglik_pseudo(theta, x_vec, yhat_label, num_cols)
}

ppp_laplace_single <- function(mu, tau2, x_vec, yhat_label, data_scaled_train, theta_start_for_tilde,num_cols, control = list(maxit = 1000, reltol = 1e-8)) {
  # Optimum \tilde{\theta} = argmax \tilde{l} (ohne Prior, gemäß deiner Approximation)
  opt_tilde <- optim(
    par = theta_start_for_tilde,
    fn  = function(th) -ltilde(th, data_scaled_train, x_vec, yhat_label, num_cols),         # negative \tilde{l}
    gr  = function(th)  grad_neg_ltilde(th, data_scaled_train, x_vec, yhat_label, num_cols),# Grad der negativen \tilde{l}
    method = "BFGS",
    control = control
  )
  theta_tilde <- opt_tilde$par
  lt_val      <- ltilde(theta_tilde, data_scaled_train, x_vec, yhat_label, num_cols)
  # Hesse der negativen \tilde{l} am Optimum (≈ observed Fisher des Integranden ohne Prior)
  H_tilde <- hessian(function(th) -ltilde(th, data_scaled_train, x_vec, yhat_label, num_cols), theta_tilde)
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

PPP_matrix <- function(priors, train_scaled, pseudolabeled_scaled) { 
  results <- list()
  predictors <- setdiff(names(pseudolabeled_scaled), "target")
  pseudo_points <- apply(pseudolabeled_scaled, 1, function(row) {
    list(
      x = as.numeric(row[predictors]),
      yhat = row[["target"]]
    )
  })
  for (pr in priors) {
    # Evidence p(D) für diesen Prior (einmalig)
    evD <- laplace_evidence_D(mu = pr$mu, tau2 = pr$tau2, theta_start = pr$mu, data_scaled_train = train_scaled)
    log_pD <- evD$log_evidence
    i = 0
    for (pp in 1:length(pseudo_points)) {
      num_cols <- as.numeric(names(pseudo_points[pp]))
      ps <- pseudo_points[[pp]]
      i = i + 1 
      # \tilde{\theta} für diesen (x_i, yhat_i); start sinnvoll (z.B. theta_pre_map)
      theta_pre_map <- rnorm(length(pr$mu), sd = 0.3)
      
      ppp_parts <- ppp_laplace_single(
        mu = pr$mu, tau2 = pr$tau2,
        x_vec = ps$x, yhat_label = ps$yhat,
        data_scaled_train = train_scaled,
        theta_start_for_tilde = theta_pre_map, num_cols
      )
      p_dim <-length(pr$mu)
      
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

M_MaxiMin_creterion <- function(matrix) {
  a <- apply(matrix, 1, min)
  a_s <- which.max(a)[1]
  return(a_s)
}
######### Analyse 
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
    geom_line(size = 1) +
    theme_minimal() +
    ggtitle(name)
  
  print(G)
  return(G)
}

######### RUN 
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

get_experiment <- function(dir = "NeuronalNet") {
  adress <- paste0("/dss/dsshome1/03/di35lox/MASTER/experiments/", dir)
  load(adress)
  return_exp <- NeuronalNet[!NeuronalNet$overall & !NeuronalNet$inProgress, ][1, ]
  if(nrow(return_exp) == 0) {
    stop("all Experiments done or in Progress")
  }
  NeuronalNet[!NeuronalNet$overall & !NeuronalNet$inProgress, ][1, ]$inProgress <- TRUE
  save(NeuronalNet, file = adress)
  
  return(return_exp)
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


experiment_to_adress <- function(exp) {
  adress <- paste0(exp$data, "_L_", exp$L, "_U_", exp$U, "_alp_", exp$alp, "_", exp$prio_t, "_", exp$prio_r)
  return(adress)
}

update_directory_strucutre <- function(dir = "NeuronalNet"){
  load("/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet")
  for(i in 1:nrow(NeuronalNet)) {
    exp <- experiment_to_adress(NeuronalNet[i,])
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
