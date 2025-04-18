library(dplyr)

set.seed(2037420)

N = 100 #wie oft wird gesampelt 

share_setups = c(0.7, 0.8, 0.9, 0.95) #wie viel Prozent sind unlabeld 

for (share_unlabeled in share_setups) {
    share_unlabeled %>% print
    try(
      source(paste(getwd(),"/benchmarks/real world data/run_benchmarks_mtcars.R", sep="")) #Ausführen
    )
    print("Benchmark fertig")
    try(
      source(paste(getwd(),"/analyze/analyze_all.R", sep=""))
    )
    print(Sys.time()) 
  }


share_setups = c(0.8, 0.9)
n_setups = c(80,120,160,200)


for (share_unlabeled in share_setups) {
  for (n in n_setups){
    share_unlabeled %>% print
    n %>% print()
    try(
      source(paste(getwd(),"/benchmarks/real world data/run_benchmarks_banknote.R", sep=""))
    )
    try(
      source(paste(getwd(),"/analyze/analyze_all.R", sep=""))
    )
    print(Sys.time()) 
  }
}

