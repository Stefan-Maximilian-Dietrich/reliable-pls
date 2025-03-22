#load Functions
source("~/Soft_Revision/NaiveBayes/Function_NB.R")


#load Data
source("~/Soft_Revision/NaiveBayes/data_NB/banknote.R")

N = 4
for(n_labled in c(4,6,8,10,12)) { 
  for(n_unlabled in c(2,4,8,16,32,64,128)) {
    for(alpha in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) {
      print(paste("RUNNING: n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
      
      core <- as.numeric(min(20, parallel::detectCores() , N)) 
      print(paste("Parallel: cores ", core))
      cl <- parallel::makeForkCluster(core)
      doParallel::registerDoParallel(cl)
      
      gamma <- foreach(m = 1:N, .combine = 'c') %dopar% {
        set.seed(m+n_labled*100+n_unlabled*10000+alpha*100000)
        source("~/Soft_Revision/NaiveBayes/NB.R")
      }
      
      path = paste(getwd(),"/NaiveBayes/results_NB/", data_name,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), sep="")
      save(gamma, file = path)
      parallel::stopCluster(cl)
      
      print(paste("DONE: n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
    }
  }
  
}