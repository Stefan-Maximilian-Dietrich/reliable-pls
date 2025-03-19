set.seed(42)  # Reproduzierbarkeit

# Simulierte Daten
n <- 100  # Anzahl der Beobachtungen
p <- 3    # Anzahl der Features (einschließlich Intercept)

X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))  # Design-Matrix mit Intercept
theta_true <- c(0.5, -1, 2)  # Wahre Parameter
probs <- 1 / (1 + exp(-X %*% theta_true))  # Wahrscheinlichkeiten
response <- rbinom(n, 1, probs)  # Binäre Antwortvariable

# Startwerte für Theta
theta_init <- rep(0, p)

# Log-Likelihood-Funktion
likelihood <- function(theta, X, response) {
  p <- 1 / (1 + exp(-X %*% theta))
  log_likelihood <- sum(response * log(p) + (1 - response) * log(1 - p))
  return(log_likelihood)
}

# Erste Ableitung (Gradient)
gradient <- function(theta, X, response) {
  p <- 1 / (1 + exp(-X %*% theta))
  grad <- t(X) %*% (response - p)
  return(grad)
}

# Zweite Ableitung (Hesse-Matrix)
hessian <- function(theta, X) {
  p <- 1 / (1 + exp(-X %*% theta))
  W <- diag(as.vector(p * (1 - p)))  # Diagonalmatrix mit p_i (1 - p_i)
  H <- -t(X) %*% W %*% X
  return(H)
}

# Berechnungen mit Startwert theta_init
loglik_val <- likelihood(theta_init, X, response)
grad_val <- gradient(theta_init, X, response)
hess_val <- hessian(theta_init, X)


otim <-  optim(theta_init, likelihood,  X= X, response= response, control = list(fnscale = -1), hessian = TRUE)


loglik_val <- likelihood(theta_true, X, response)
grad_val <- gradient(otim$par, X, response)
hess_val <- hessian(otim$par, X)

# Ergebnisse ausgeben
cat("Log-Likelihood: ", loglik_val, "\n")
cat("Gradient:\n")
print(grad_val)
cat("Hesse-Matrix:\n")
print(hess_val)
