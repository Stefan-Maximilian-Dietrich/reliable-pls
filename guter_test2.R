## ============================================================
## PPP via Laplace für viele (x_i, yhat_i) und viele Prioris
## ============================================================

set.seed(123)

## -------------------------
## 0) Daten (Iris) & Split
## -------------------------
data(iris)

# 70/30 stratifizierter Split
idx_by_class <- split(seq_len(nrow(iris)), iris$Species)
train_idx <- unlist(lapply(idx_by_class, function(ix) sample(ix, round(0.7 * length(ix)))))
train <- iris[train_idx, ]
test  <- iris[-train_idx, ]

# Skalierung anhand Training
num_cols <- names(iris)[1:4]
train_x <- scale(train[, num_cols])
scale_center <- attr(train_x, "scaled:center")
scale_scale  <- attr(train_x, "scaled:scale")
test_x  <- scale(test[, num_cols], center = scale_center, scale = scale_scale)

train_scaled <- data.frame(train_x, Species = droplevels(train$Species))
test_scaled  <- data.frame(test_x,  Species = droplevels(test$Species))

## ----------------------------------------
## 1) Feste Architektur & NN-Grundbausteine
## ----------------------------------------
classes <- levels(train_scaled$Species)
d <- length(num_cols)   # 4 Inputs
h <- 5                  # Hidden Units (fix)
K <- length(classes)    # 3 Klassen
p_dim <- h*d + h + K*h + K  # Anzahl Parameter

unpack_theta <- function(theta) {
  stopifnot(length(theta) == p_dim)
  ofs <- 0
  W1 <- matrix(theta[(ofs + 1):(ofs + h*d)], nrow = h, ncol = d); ofs <- ofs + h*d
  b1 <- theta[(ofs + 1):(ofs + h)]; ofs <- ofs + h
  W2 <- matrix(theta[(ofs + 1):(ofs + K*h)], nrow = K, ncol = h); ofs <- ofs + K*h
  b2 <- theta[(ofs + 1):(ofs + K)]
  list(W1=W1, b1=b1, W2=W2, b2=b2)
}
pack_theta <- function(W1,b1,W2,b2) c(as.vector(W1), as.vector(b1), as.vector(W2), as.vector(b2))

sigmoid <- function(z) 1/(1+exp(-z))
softmax_rows <- function(Z) {
  zmax <- apply(Z, 1, max)
  Zs   <- sweep(Z, 1, zmax, "-")
  EZ   <- exp(Zs)
  sweep(EZ, 1, rowSums(EZ), "/")
}

forward_full <- function(X, W1, b1, W2, b2) {
  n <- nrow(X)
  Hlin <- X %*% t(W1) + matrix(b1, n, h, byrow = TRUE) # n x h
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(W2) + matrix(b2, n, K, byrow = TRUE) # n x K
  P    <- softmax_rows(Z)
  list(Hlin=Hlin, H=H, Z=Z, P=P)
}

forward_probs_theta <- function(X, theta) {
  pr <- unpack_theta(theta)
  n <- nrow(X)
  Hlin <- X %*% t(pr$W1) + matrix(pr$b1, n, h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(pr$W2) + matrix(pr$b2, n, K, byrow = TRUE)
  softmax_rows(Z)
}

## --------------------------------
## 2) Log-Likelihood-Grundfunktionen
## --------------------------------
loglik_data <- function(theta, data_scaled) {
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  y <- factor(data_scaled$Species, levels = classes)
  P <- forward_probs_theta(X, theta)
  colnames(P) <- classes
  eps <- .Machine$double.eps
  P <- pmax(pmin(P, 1 - eps), eps)
  idx <- cbind(seq_len(nrow(X)), as.integer(y))
  sum(log(P[idx]))
}

# Grad der negativen Log-Likelihood für ein (Teil-)Datenset
grad_neg_loglik_data <- function(theta, data_scaled) {
  pr <- unpack_theta(theta)
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  y <- factor(data_scaled$Species, levels = classes)
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

## -------------------------------------------
## 3) PPP-Teil: \tilde{l}, ihr Grad, und Hessian
##     \tilde{l} = l_{D ∪ (x_i, yhat)} + l_D = 2*l_D + l_pseudo
## -------------------------------------------
make_single_df <- function(x_vec, yhat_label) {
  stopifnot(length(x_vec) == d)
  df <- as.data.frame(matrix(x_vec, nrow = 1))
  names(df) <- num_cols
  df$Species <- factor(yhat_label, levels = classes)
  df
}

loglik_pseudo <- function(theta, x_vec, yhat_label) {
  df1 <- make_single_df(x_vec, yhat_label)
  loglik_data(theta, df1)
}

ltilde <- function(theta, data_scaled_train, x_vec, yhat_label) {
  2 * loglik_data(theta, data_scaled_train) + loglik_pseudo(theta, x_vec, yhat_label)
}

# Grad der negativen \tilde{l} (für BFGS)
grad_neg_ltilde <- function(theta, data_scaled_train, x_vec, yhat_label) {
  df1 <- make_single_df(x_vec, yhat_label)
  2 * grad_neg_loglik_data(theta, data_scaled_train) + grad_neg_loglik_data(theta, df1)
}

## ------------------------------
## 4) Prior, Evidence p(D) (Laplace)
## ------------------------------
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

suppressPackageStartupMessages(library(numDeriv))

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

laplace_evidence_D <- function(mu, tau2, theta_start, data_scaled_train,
                               control = list(maxit = 1000, reltol = 1e-8)) {
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

## -------------------------------------------------
## 5) PPP per Prior & neuem Punkt (Laplace um \tilde{l})
## -------------------------------------------------
ppp_laplace_single <- function(mu, tau2, x_vec, yhat_label, data_scaled_train, theta_start_for_tilde,
                               control = list(maxit = 1000, reltol = 1e-8)) {
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

## ---------------------------------------------------------
## 6) Beispiel-Prioris (gute & schlechte) und Pseudo-Samples
## ---------------------------------------------------------
set.seed(777)

# Vorbereitende "weite" MAP (nur um sinnvolle Zentren zu konstruieren)
theta0 <- rnorm(p_dim, sd = 0.3)
tmp <- laplace_evidence_D(mu = rep(0, p_dim), tau2 = 10, theta_start = theta0, data_scaled_train = train_scaled)
theta_pre_map <- tmp$theta_mapD

# Problematische Mittelwerte (schlechte Priors)
mu_wrong_1 <- -theta_pre_map
mu_wrong_2 <- theta_pre_map + rnorm(p_dim, sd = 1.5)

priors <- list(
  # tendenziell "gute"
  list(name="wide_zero",            mu=rep(0, p_dim),     tau2=10),
  list(name="moderate_zero",        mu=rep(0, p_dim),     tau2=1),
  list(name="tight_map_centered",   mu=theta_pre_map,     tau2=0.05),
  list(name="moderate_map_centered",mu=theta_pre_map,     tau2=0.5),
  # tendenziell "schlechte"
  list(name="ultra_tight_zero",     mu=rep(0, p_dim),     tau2=0.0025),
  list(name="tight_wrong_mean_1",   mu=mu_wrong_1,        tau2=0.05),
  list(name="moderate_wrong_mean_2",mu=mu_wrong_2,        tau2=0.5)
)

# Wähle einige neue Punkte x_i aus dem Testset (eine pro Klasse)
ix_setosa     <- which(test_scaled$Species=="setosa")[1]
ix_versicolor <- which(test_scaled$Species=="versicolor")[1]
ix_virginica  <- which(test_scaled$Species=="virginica")[1]

x_setosa     <- as.numeric(test_scaled[ix_setosa, 1:d])
x_versicolor <- as.numeric(test_scaled[ix_versicolor, 1:d])
x_virginica  <- as.numeric(test_scaled[ix_virginica, 1:d])

# Für jeden x_i je ein "wahrscheinliches" Label (korrekt) und ein "unwahrscheinliches" (falsch)
pseudo_points <- list(
  list(name="x_setosa_correct",      x=x_setosa,     yhat="setosa"),
  list(name="x_setosa_wrong",        x=x_setosa,     yhat="virginica"),
  list(name="x_versicolor_correct",  x=x_versicolor, yhat="versicolor"),
  list(name="x_versicolor_wrong",    x=x_versicolor, yhat="setosa"),
  list(name="x_virginica_correct",   x=x_virginica,  yhat="virginica"),
  list(name="x_virginica_wrong",     x=x_virginica,  yhat="versicolor")
)

## ------------------------------------------------------
## 7) Doppelschleife: für jeden Prior & jeden (x_i, yhat_i)
##     PPP ≈ exp(ltilde) π(θ~) / p(D) * (2π)^{p/2} |H|^{-1/2}
## ------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))

results <- list()

for (pr in priors) {
  # Evidence p(D) für diesen Prior (einmalig)
  evD <- laplace_evidence_D(mu = pr$mu, tau2 = pr$tau2, theta_start = pr$mu, data_scaled_train = train_scaled)
  log_pD <- evD$log_evidence
  
  for (ps in pseudo_points) {
    # \tilde{\theta} für diesen (x_i, yhat_i); start sinnvoll (z.B. theta_pre_map)
    ppp_parts <- ppp_laplace_single(
      mu = pr$mu, tau2 = pr$tau2,
      x_vec = ps$x, yhat_label = ps$yhat,
      data_scaled_train = train_scaled,
      theta_start_for_tilde = theta_pre_map
    )
    
    # Log-PPP gemäß deiner Approximation
    log_PPP <- ppp_parts$log_ltilde + ppp_parts$log_prior_tilde - log_pD +
      0.5 * p_dim * log(2*pi) - 0.5 * ppp_parts$logdetH_tilde
    
    results[[length(results)+1]] <- data.frame(
      prior      = pr$name,
      tau2       = pr$tau2,
      pseudo     = ps$name,
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

res_df <- bind_rows(results) %>%
  arrange(desc(log_PPP))

print(res_df, digits = 4, row.names = FALSE)

cat("\nTop 5 PPP (größte log_PPP):\n")
print(head(res_df, 5), digits = 4, row.names = FALSE)

cat("\nBottom 5 PPP (kleinste log_PPP):\n")
print(tail(res_df, 5), digits = 4, row.names = FALSE)
