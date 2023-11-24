
install.packages("mnormt")
install.packages("mvtnorm")
install.packages("mmeln")
install.packages("mvnfast")



library(mnormt)
library(mvtnorm)
library(mmeln)
library(mvnfast)



sigma_priori <-  rbind(beta0 = c(7,0,0,0),cbind(beta0 = c(0,0,0), cov(data_frame[c(7,5,2)]) ))

#3
profvis({
  for( i in 1:100000)
    priori_mnormt <- c(priori_mnormt, mnormt::dmnorm(x = c(1,i*2,3,i), mean = c(0,i,i,i), varcov = sigma_priori))
})


#4
profvis({
  for( i in 1:100000)
    priori_mvtnorm <- c(priori_mvtnorm , mvtnorm::dmvnorm(x = c(1,i*2,3,i), mean = c(0,i,i,i), sigma = sigma_priori))
})

#2
profvis({
  for( i in 1:100000)
    priori_mmeln <- c(priori_mmeln, mmeln::dmnorm(X = c(1,i*2,3,i), Mu = c(0,i,i,i), Sigma = sigma_priori))
})

#1
profvis({
  for( i in 1:100000)
    priori_mvnfast <- c(priori_mvnfast, mvnfast::dmvn(X = c(1,i*2,3,i), mu = c(0,i,i,i), sigma = sigma_priori))
  
})

View(cbind(priori_mnormt, priori_mvtnorm, priori_mmeln, priori_mvnfast))
