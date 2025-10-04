## ========= 0) Setup & Daten =========
set.seed(123)
data(iris)

# 70/30 stratifizierter Split
idx_by_class <- split(seq_len(nrow(iris)), iris$Species)
train_idx <- unlist(lapply(idx_by_class, function(ix) sample(ix, round(0.7 * length(ix)))))
train <- iris[train_idx, ]
test  <- iris[-train_idx, ]

# Skalierung (nur mit Trainingsstatistiken)
num_cols <- names(iris)[1:4]
train_x <- scale(train[, num_cols])
scale_center <- attr(train_x, "scaled:center")
scale_scale  <- attr(train_x, "scaled:scale")
test_x  <- scale(test[, num_cols], center = scale_center, scale = scale_scale)

train_scaled <- data.frame(train_x, Species = droplevels(train$Species))
test_scaled  <- data.frame(test_x,  Species = droplevels(test$Species))

# Klassen & Dimensionen (KONSTANTE ARCHITEKTUR)
classes <- levels(train_scaled$Species)
d <- length(num_cols)   # 4
K <- length(classes)    # 3
h <- 5                  # Hidden Units (fix)

## ========= 1) Hilfsfunktionen (Pack/Unpack, Forward, Softmax, Likelihood) =========
theta_length <- h*d + h + K*h + K  # W1, b1, W2, b2

unpack_theta <- function(theta) {
  stopifnot(length(theta) == theta_length)
  ofs <- 0
  W1 <- matrix(theta[(ofs + 1):(ofs + h*d)], nrow = h, ncol = d); ofs <- ofs + h*d
  b1 <- theta[(ofs + 1):(ofs + h)]; ofs <- ofs + h
  W2 <- matrix(theta[(ofs + 1):(ofs + K*h)], nrow = K, ncol = h); ofs <- ofs + K*h
  b2 <- theta[(ofs + 1):(ofs + K)]
  list(W1 = W1, b1 = b1, W2 = W2, b2 = b2)
}

pack_theta <- function(W1, b1, W2, b2) {
  c(as.vector(W1), as.vector(b1), as.vector(W2), as.vector(b2))
}

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

## Likelihood (nur von theta abhängig) – kompatibel zu vorherigem Design
forward_probs_theta <- function(X, theta) {
  pr <- unpack_theta(theta)
  Hlin <- X %*% t(pr$W1) + matrix(pr$b1, nrow(X), h, byrow = TRUE)
  H    <- sigmoid(Hlin)
  Z    <- H %*% t(pr$W2) + matrix(pr$b2, nrow(X), K, byrow = TRUE)
  softmax_rows(Z)
}

loglik_theta <- function(theta, data_scaled) {
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  y <- factor(data_scaled$Species, levels = classes)
  P <- forward_probs_theta(X, theta)
  colnames(P) <- classes
  eps <- .Machine$double.eps
  P <- pmax(pmin(P, 1 - eps), eps)
  idx <- cbind(seq_len(nrow(X)), as.integer(y))
  ll  <- log(P[idx])
  list(logLik_sum = sum(ll), logLik_avg = mean(ll), n = length(ll))
}

predict_class_theta <- function(theta, data_scaled) {
  X <- as.matrix(data_scaled[, 1:d, drop = FALSE])
  P <- forward_probs_theta(X, theta)
  factor(classes[max.col(P, ties.method = "first")], levels = classes)
}

## ========= 2) Manuelle Schätzung per Gradient Descent (Softmax-Cross-Entropy) =========
# One-hot Kodierung
one_hot <- function(y, class_levels) {
  y <- factor(y, levels = class_levels)
  Y <- matrix(0, nrow = length(y), ncol = length(class_levels))
  Y[cbind(seq_along(y), as.integer(y))] <- 1
  colnames(Y) <- class_levels
  Y
}

Xtr <- as.matrix(train_scaled[, 1:d, drop = FALSE])
Ytr <- one_hot(train_scaled$Species, classes)

# Initialisierung
set.seed(42)
W1 <- matrix(rnorm(h * d, sd = 0.3), nrow = h, ncol = d)
b1 <- rnorm(h, sd = 0.3)
W2 <- matrix(rnorm(K * h, sd = 0.3), nrow = K, ncol = h)
b2 <- rnorm(K, sd = 0.3)

# Hyperparameter
lr      <- 0.05     # Lernrate
epochs  <- 400      # Epochen
lambda  <- 1e-3     # L2-Regularisierung (Gewichte)
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

## ========= 3) Trainiertes theta erzeugen & in Likelihood/Prediction einsetzen =========
theta_hat <- pack_theta(W1, b1, W2, b2)

# Log-Likelihood auf Train/Test mit dem manuell geschätzten theta
ll_tr <- loglik_theta(theta_hat, train_scaled)
ll_te <- loglik_theta(theta_hat, test_scaled)
print(ll_tr); print(ll_te)

# Radom Theta 
theta_random <- runif(theta_length, min = -2, max = 2)

ll_tr_rand <- loglik_theta(theta_random, train_scaled)
ll_te_rand <- loglik_theta(theta_random, test_scaled)
print(ll_tr_rand); print(ll_te_rand)

# Vorhersage & Konfusionsmatrix (Test)
pred_te <- predict_class_theta(theta_hat, test_scaled)
cm <- table(Truth = test_scaled$Species, Pred = pred_te)
acc <- sum(diag(cm)) / sum(cm)

# Vorhersage & Konfusionsmatrix von Zufälligen Theta (Test)
pred_te_rand <- predict_class_theta(theta_random, test_scaled)
cm_rand <- table(Truth = test_scaled$Species, Pred = pred_te_rand)
acc_rand <- sum(diag(cm_rand)) / sum(cm_rand)

print(acc)
print(acc_rand)

cat("\nKonfusionsmatrix (Test):\n"); print(cm)
cat(sprintf("\nAccuracy (Test): %.3f\n", acc))
