culm <- NULL
for(e in 1:100){
  source("Naiv_Bayes.R")
  culm <- rbind(culm, result)
  View(culm)
}