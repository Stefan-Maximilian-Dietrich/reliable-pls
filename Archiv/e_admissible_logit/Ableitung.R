# Ableitung 
# h' = log(L*p)' 
# h' = (log(L) + log(p))' 
# h' = log(L)' + log(p)' 
# h' = L'*1/L + p'*1/p 
# h' = 0



# h = log(L) + log(p)

log_Likelihood <- function(X,Y,theta) {
  LL <-sum((X %*% theta)*Y - log(1 + exp(X %*% theta)))
  return(LL)
}

log_priori <- function(theta, mu_priori, sigma_priori) {
  LP <- log(mvnfast::dmvn(X = theta, mu = mu_priori, sigma = sigma_priori))
  return(LP)
}

deriv_log_Likelihood <- function(X,Y,theta){
  dLL <- rowsum(Y *  X - 1 / (1 + exp(-X %*% theta) * X))
  return(dLL)
}

deriv_log_priori <- function(theta, mu_priori, sigma_priori){
  dLP <- - 1/2 * (theta - mu_priori) * (inv(sigma_priori) + t(inv(sigma_priori)))
  return(dLP)
}

h <- function(X,Y, theta, mu_priori, sigma_priori) {
  H <- log_Likelihood(X=X,Y=Y,theta=theta) + log_priori(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  return(H)
}

deriv_h <- function(X,Y, theta, mu_priori, sigma_priori){
  dH <- deriv_log_Likelihood(X=X,Y=Y,theta=theta) + deriv_log_priori(theta = theta, mu_priori = mu_priori, sigma_priori = sigma_priori)
  return(dH)
}

