calculateML <- function(priori, log_likelihood, boundary) {
  prior <- createPrior(density = priori[[1]], sampler = priori[[2]], lower = boundary[[1]], upper = boundary[[2]])
  setUp <- createBayesianSetup(likelihood = log_likelihood, prior = prior)
  out <- runMCMC(bayesianSetup = setUp, sampler = "DEzs", settings = list(iterations = 10000))
  log_ML <- marginalLikelihood(out, start = 1000)  # Ignoriere die ersten 500 Iterationen
  ML <- exp(log_ML$ln.ML)
  return(ML)
}

priori <- list()
priori[[2]] <-   function(n = 1) { mvnfast::rmvn(n = n, mu = c(1,1,1), sigma = diag(c(1,1,1)))}

priori[[1]]   <- function(theta) { mvnfast::dmvn(X = theta, mu = c(1,1,1), sigma = diag(c(1,1,1)))}

log_likelihood <- function(theta) {log( mvnfast::dmvn(X = theta, mu = c(1,1,1), sigma = diag(c(1,1,1))))}

boundary <- list(-5*c(1,1,1), 5*c(1,1,1))

calculateML(priori, log_likelihood, boundary)