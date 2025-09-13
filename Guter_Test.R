## ============================================================
## Laplace-Evidence für ein fixes NN (Iris) über mehrere Normal-Prioris
## ============================================================

## 0) Daten vorbereiten (Iris)
set.seed(123)
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

## 1) Feste Architektur & Basisfunktionen
classes <- levels(train_scaled$Species)
d <- length(num_cols)   # 4 Inputs
h <- 5                  # Hidden Units (fix)
K <- length(classes)    # 3 Klassen

theta_length <- h*d + h + K*h + K  # W1(h*d) + b1(h) + W2(K*h) + b2(K)

unpack_theta <- function(theta) {
  stopifnot(length(theta) == theta_length)
  ofs <- 0
  W1 <- matrix(theta[(ofs + 1):(ofs + h*d)], nrow = h, ncol = d); ofs <- ofs + h*d
  b1 <- theta[(ofs + 1):(ofs + h)]; ofs <- ofs + h
  W2 <- matrix(theta[(ofs + 1):(ofs + K*h)], nrow = K, ncol = h); ofs <- ofs + K*h
  b2 <- theta[(ofs + 1):(ofs + K)]
  list(W1 = W1, b1 = b1, W2 = W2, b2 = b2)
}
pack_theta <- function(W1, b1, W2, b2) c(as.vector(W1), as.vector(b1), as.vector(W2), as.vector(b2))

sigmoid <- function(z) 1 / (1 + exp(-z))
softmax_rows <- function(Z) {
  zmax <- apply(Z, 1, max)
  Zs   <- sweep(Z, 1, zmax, "-")
  EZ   <- exp(Zs)
  sweep(EZ, 1, rowSums(EZ), "/")
}

forward_full <- function(X, W1, b1, W2, b2) {
  n <- nrow(X)
  Hlin <- X %*% t(W1) + matrix(b1, n, h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(W2) + matrix(b2, n, K, byrow = TRUE)
  P    <- softmax_rows(Z)
  list(Hlin = Hlin, H = H, Z = Z, P = P)
}

forward_probs_theta <- function(X, theta) {
  pr <- unpack_theta(theta)
  n <- nrow(X)
  Hlin <- X %*% t(pr$W1) + matrix(pr$b1, n, h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(pr$W2) + matrix(pr$b2, n, K, byrow = TRUE)
  softmax_rows(Z)
}

## Log-Likelihood (Summe) – gleiche Likelihood für alle Prioris
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

## 2) Normal-Prior N(mu, tau2 I) & (neg.) Log-Posterior + Gradient
log_prior_normal <- function(theta, mu, tau2) {
  p <- length(theta)
  r <- theta - mu
  -0.5 * (sum(r^2) / tau2) - 0.5 * p * (log(2*pi) + log(tau2))
}

# Exakter Gradient der negativen Log-Posterior
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

## 3) Numerische Hesse & stabile Log-Determinante
suppressPackageStartupMessages(library(numDeriv))

robust_logdet_pd <- function(H) {
  # Versuche Cholesky; bei Bedarf Jitter hinzufügen
  jit_vals <- c(0, 1e-8, 1e-6, 1e-4, 1e-3)
  for (jit in jit_vals) {
    Hjit <- if (jit > 0) H + diag(jit, nrow(H)) else H
    ok <- TRUE
    ch <- tryCatch(chol(Hjit), error = function(e) { ok <<- FALSE; NULL })
    if (ok) {
      return(list(logdet = 2 * sum(log(diag(ch))), jitter = jit, chol = ch))
    }
  }
  # Fallback (nicht ideal, wenn H nicht PD ist)
  ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  ev_pos <- pmax(ev, 1e-10) # clamp
  list(logdet = sum(log(ev_pos)), jitter = NA_real_, chol = NULL)
}

## 4) Laplace-Evidence für EIN Prior
laplace_evidence_for_prior <- function(prior_name, mu, tau2, theta_init, data_scaled_train, data_scaled_test = NULL,
                                       control = list(maxit = 1000, reltol = 1e-8)) {
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

## 5) Beispiel-Prioris erzeugen (gute & schlechte)
set.seed(777)

# 5a) Vorläufige "weite" MAP, um einen sinnvollen Mittelpunkt zu bekommen
theta0 <- rnorm(theta_length, sd = 0.3)
tau2_wide <- 10
opt_wide <- optim(
  par = theta0,
  fn  = function(th) neg_log_post(th, train_scaled, mu = rep(0, theta_length), tau2 = tau2_wide),
  gr  = function(th) grad_neg_log_post(th, train_scaled, mu = rep(0, theta_length), tau2 = tau2_wide),
  method = "BFGS",
  control = list(maxit = 1000, reltol = 1e-8)
)
theta_pre_map <- opt_wide$par

# 5b) Erzeuge "schlechte" Mittelwerte (weit weg)
mu_wrong_1 <- -theta_pre_map                      # gegenüber Vorzeichen gespiegelt
mu_wrong_2 <- theta_pre_map + rnorm(theta_length, sd = 1.5)  # stark verschoben

# 5c) Liste von Prioris (Name, mu, tau2)
priors <- list(
  # tendenziell "gut": breit oder gut zentriert
  list(name = "wide_zero",          mu = rep(0, theta_length),           tau2 = 10),
  list(name = "moderate_zero",      mu = rep(0, theta_length),           tau2 = 1),
  list(name = "tight_map_centered", mu = theta_pre_map,                  tau2 = 0.05),
  list(name = "moderate_map_centered", mu = theta_pre_map,               tau2 = 0.5),
  
  # tendenziell "schlecht": sehr eng oder falsches Zentrum
  list(name = "ultra_tight_zero",   mu = rep(0, theta_length),           tau2 = 0.0025),
  list(name = "tight_wrong_mean_1", mu = mu_wrong_1,                     tau2 = 0.05),
  list(name = "moderate_wrong_mean_2", mu = mu_wrong_2,                  tau2 = 0.5)
)

## 6) Schleife über alle Prioris – Evidence berechnen
suppressPackageStartupMessages(library(dplyr))

results <- lapply(priors, function(pr) {
  # sinnvolles Start-Theta: Prior-Mittelwert
  theta_start <- pr$mu
  laplace_evidence_for_prior(
    prior_name = pr$name,
    mu         = pr$mu,
    tau2       = pr$tau2,
    theta_init = theta_start,
    data_scaled_train = train_scaled,
    data_scaled_test  = test_scaled
  )
})

## 7) Ergebnisse zusammenfassen & sortieren
res_df <- bind_rows(lapply(results, function(r) data.frame(
  prior        = r$name,
  tau2         = r$tau2,
  mu_norm      = r$mu_norm,
  converged    = r$map_converged,
  loglik_map   = r$loglik_map,
  logprior_map = r$logprior_map,
  logdetH      = r$logdetH,
  jitter       = ifelse(is.na(r$jitter_used), NA, r$jitter_used),
  log_evidence = r$log_evidence,
  acc_test     = r$acc_test
)))

res_df <- res_df %>%
  arrange(desc(log_evidence))

print(res_df, digits = 4, row.names = FALSE)

## 8) (Optional) Bestes/Schlechtestes Prior anzeigen
cat("\nBestes Prior (nach Log-Evidence):\n")
print(res_df[1, , drop = FALSE])

cat("\nSchlechtestes Prior (nach Log-Evidence):\n")
print(res_df[nrow(res_df), , drop = FALSE])

ppp_m
df <- do.call(rbind, lapply(ppp_m, as.data.frame))

