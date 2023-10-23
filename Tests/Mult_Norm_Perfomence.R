
install.packages("mnormt")
install.packages("mvtnorm")
install.packages("mmeln")



library(mnormt)
library(mvtnorm)
library(mmeln)


sigma_priori <-  rbind(beta0 = c(7,0,0,0),cbind(beta0 = c(0,0,0), cov(data_frame[c(7,5,2)]) ))

#2
profvis({
  for( i in 1:100000)
    mnormt::dmnorm(x = c(1,i*2,3,i), mean = c(0,i,i,i), varcov = sigma_priori)
})


#3
profvis({
  for( i in 1:100000)
    mvtnorm::dmvnorm(x = c(1,i*2,3,i), mean = c(0,i,i,i), sigma = sigma_priori)
})

#1
profvis({
  for( i in 1:100000)
    mmeln::dmnorm(X = c(1,i*2,3,i), Mu = c(0,i,i,i), Sigma = sigma_priori)
})



View(cbind(priori_mnormt, priori_mvtnorm, priori_mmeln))
