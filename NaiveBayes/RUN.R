### setup Session 
source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))

### run Experiments
workers = 4
label <- c(4, 6)
unlabel <- c(4)
alp <- c(0.9)
N = 4
used_data <- c("ionosphere")
source(paste(getwd(),"/NaiveBayes/run_benchmark_nb.R", sep = ""))

### analyse Results
online <- FALSE #Wenn TRUE anaylse dirket im LRZ Cluster 
make_all_Graphics(online) 
show_all_Results(online)
show_Summary(online)
