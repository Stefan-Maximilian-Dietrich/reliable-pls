install.packages("rbenchmark")
install.packages("fields")


library('RcppArmadillo')
library('mvtnorm')
library('rbenchmark')
library('fields')

dmvnorm_deriv1 <- function(x, mu=rep(0,ncol(X)), sigma=diag(ncol(X))) {
  fn <- function(x) -1 * c((1/sqrt(det(2*pi*sigma))) * exp(-0.5*t(x-mu)%*%solve(sigma)%*%(x-mu))) * solve(sigma,(x-mu))
  out <- fn()
  return(out)
}





fn(x= c(0,1), mu=c(0,8), sigma = matrix(c(5,1,2,3), nrow=2))



fn <- function(x, mu, sigma){
  -1 * c((1/sqrt(det(2*pi*sigma))) * exp(-0.5*t(x-mu)%*%solve(sigma)%*%(x-mu))) * solve(sigma,(x-mu))
} 

gn <- function(theta, Y, X) {

    odds <- exp(X %*% theta)
    prob <- odds / (1 + odds)
    
    print(t(Y- prob)%*%X)

  
    t(Y- prob)%*%X * likelihood_function(X, theta = theta, response = Y)
}

derivate <- function(x, mu, sigma, X, Y) {
  priori_der <- fn(x = x, mu = mu, sigma = sigma) 
  likelihood <- likelihood_function(X = X, theta = x, response = Y) 
  priori <- mvnfast::dmvn(X = x, mu = mu, sigma = sigma) 
  likelihood_der <- gn(theta = x, Y = Y, X=X)
  print(paste("priori_der", priori_der))
  print(paste("likelihood", likelihood))
  print(paste("priori", priori))
  print(paste("likelihood_der", likelihood_der))
  priori_der * likelihood + priori * likelihood_der
}

mu = c(1,2)
sigma = matrix(c(5,1,2,3), nrow=2)
Y <- as.matrix(data100$y)
X <- cbind(1, data100$x1)


optim(par = c(0.0), )

derivate(x = c(0, 0), mu = mu, sigma = sigma, X = X, Y  = Y)


logistic_model <- glm(formula = y ~ x1, 
                      data = as.data.frame(X), 
                      family = "binomial")