# berechne m Tensor für alle 
library(data.table)
Limits <- matrix(c(-5,5,2.5,
                       -5,5,2.5,
                       -5,5,2.5,
                       -5,5,2.5), ncol = 3, byrow = TRUE)

x1 <- rnorm(10000, mean = 0, sd = 1)
x2 <- rnorm(10000, mean = 2, sd = 5)
x3 <- rnorm(10000, mean = 1, sd = 2)

lin_comb <- -0.5 - 1*x1 +  2.8*x2 + 0.4*x3

prob = 1/(1+exp(-lin_comb))

y <-rbinom(10000, 1, prob = prob)


df <- as.data.frame(cbind(y, x1, x2, x3))
data100 <- df[1:100, ]
response <- data100$y
data_matrix <- data100[c(2,3,4)]
sigma_priori  <- rbind(beta0 = c(1,0,0,0),cbind(beta0 = c(0,0,0), cov(data100[c(2,3,4)]) ))


names_function <- function(Limits) {
  p <- nrow(Limits)
  names <- c()
  for(i in 0:(p-1)) {
    names <- c(names, paste0("beta",i))
  }
  return(names)
}
   
sequence_function <- function(Limits) {
  lower <- Limits[, 1]
  upper <- Limits[, 2]
  step <- Limits[, 3]

  p <- nrow(Limits)
  
  core <- min(as.numeric(parallel::detectCores() - 1), as.numeric(p))
  cl <- parallel::makeForkCluster(core)
  doParallel::registerDoParallel(cl)
  sequence <- foreach(i = 1:p) %dopar% {
    seq(from = lower[i], to = upper[i], by = step[i])
  }
  parallel::stopCluster(cl)

  return(sequence)
}

grid_function <- function(sequences) {
  grid <- expand.grid(sequences)
  return(grid)
}

get_variable_grid <- function(Limits) {
  names <- names_function(Limits)
  sequences <- sequence_function(Limits)
  grid <- grid_function(sequences)
  names(grid) <- names
  return(grid)
}

get_m_grid <- function(grid) {
  n <- nrow(grid)
  core <- min(as.numeric(parallel::detectCores() - 1), as.numeric(n))
  #core <- as.numeric(1)
  cl <- parallel::makeForkCluster(core)
  doParallel::registerDoParallel(cl)
  m_grid <- foreach(i = 1:n) %dopar% {
    m_mu_function(data_matrix = data_matrix, response = response, mu_priori = unlist(grid[i,]), sigma_priori = sigma_priori)   }
  parallel::stopCluster(cl)
  m_grid <- cbind(grid, m = unlist(m_grid))
  
  return(m_grid)
}

alpha_cut_m <- function(m_grid, alpha) {
  max <- max(m_grid$m)
  m_grid[m_grid$m < alpha * max, ]$m <- NA
  return(m_grid)
}

get_eu_grid <- function(grid) {
  n <- nrow(grid)
  core <- min(as.numeric(parallel::detectCores() - 1), as.numeric(n))
  cl <- parallel::makeForkCluster(core)
  doParallel::registerDoParallel(cl)
  eu_grid <- foreach(i = 1:n) %dopar% {
    expected_utility_function(data_matrix = data_matrix, response = response , mu_priori = unlist(grid[i,]), sigma_priori = sigma_priori) } 
  parallel::stopCluster(cl)
  eu_grid <- cbind(grid, expected_utility = unlist(eu_grid))
  return(eu_grid)
}

alpha_cut_eu <- function(acm_grid, eu_grid) {
  boolean <- is.na(acm_grid$m) 
  eu_grid[boolean, ]$expected_utility <- NA
  return(eu_grid)
}

start <- Sys.time()
grid_vars <- get_variable_grid(Limits)
m_grid <- get_m_grid(grid_vars)
ac_m_grid <- alpha_cut_m(m_grid, 0.5)
eu_grid <- get_eu_grid(grid_vars)
ac_eu_grid <- alpha_cut_eu(ac_m_grid, eu_grid)

end <- Sys.time()
end - start

