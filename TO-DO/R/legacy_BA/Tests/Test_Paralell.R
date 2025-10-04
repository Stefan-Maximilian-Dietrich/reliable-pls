install.packages("foreach")
library("foreach")
library("doParallel")


fun2 <- function(i) {
  result <- sqrt(sqrt(i)*10+10^i)
  message("Dies wird sofort angezeigt.")
  flush.console()
  
  return(result)
}

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
foreach(i = 1:5000, .combine = 'c') %dopar% {
  fun2(Data[i])
}
parallel::stopCluster(cl)


Data <- rnorm(5000)

cl <- parallel::makeForkCluster(2)
doParallel::registerDoParallel(cl)
A <- foreach(i = 1:5000, .combine = 'c') %dopar% {
  fun2(Data[i])
  message("Dies wird sofort angezeigt.")
}
parallel::stopCluster(cl)


A = list(df[1:100, ], df[101:200, ], df[201:500, ], df[301:400, ])

start_time <- Sys.time()
cl <- parallel::makeForkCluster(8)
doParallel::registerDoParallel(cl)
B <- foreach(i = 1:4, .combine = 'c') %dopar% {
  gamma_maximin_alpaC_addapter(data = A[[i]], glm_formula = formula, target = "y", mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)
}
parallel::stopCluster(cl)
end_time <- Sys.time()

end_time - start_time