# To DO bessere möglichkeit um marginale likelihood zu berechnen 

library(BayesianTools)

# 1. Prior erstellen
# Definiere die Prior-Verteilung als eine uniforme Verteilung
density <- function(par) {
  dunif(par[1], min = -10, max = 10, log = TRUE) + # Prior für mu
    dunif(par[2], min = 0.1, max = 10, log = TRUE) # Prior für sigma
}


sampler <- function(n = 1) {
  cbind(runif(n, min = -10, max = 10), runif(n, min = 0.1, max = 10))
}
mu_priori_lower <- c(-10, 0.1)
mu_priori_upper <- c(10, 10)

prior <- createPrior(density = density, sampler = sampler, 
                     lower = mu_priori_lower, upper = mu_priori_upper)

# 2. Likelihood erstellen
# Beispiel-Daten: Normalverteilte Daten mit mu = 5 und sigma = 2
set.seed(123)
data <- rnorm(100, mean = 5, sd = 2)

log_likelihood <- function(par) {
  mu <- par[1]
  sigma <- par[2]
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# 3. Bayesian Setup erstellen
setUp <- createBayesianSetup(likelihood = log_likelihood, prior = prior)

# 4. MCMC ausführen
out <- runMCMC(bayesianSetup = setUp, sampler = "DEzs", 
               settings = list(iterations = 5000))

# 5. Marginale Likelihood berechnen
log_ML <- marginalLikelihood(out, start = 500)  # Ignoriere die ersten 500 Iterationen
ML <- exp(log_ML$ln.ML)

# Ergebnisse anzeigen
cat("Log Marginale Likelihood:", log_ML$ln.ML, "\n")
cat("Marginale Likelihood:", ML, "\n")