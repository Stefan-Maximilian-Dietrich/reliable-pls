#load Functions
source(paste(getwd(),"/NaiveBayes/Function_NB.R", sep = ""))
source(paste(getwd(),"/NaiveBayes/Methods.R", sep = ""))


label <- c(5)
unlabel <- c(8)
alp <- c(0.6)
N = 8
used_data <- c("banknote")

for(dat in used_data) {
  for(n_labled in label) { 
    for(n_unlabled in unlabel) {
      for(alpha in alp) {
        print(paste("RUNNING: n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
        data_loader(dat)
        core <- as.numeric(min(20, parallel::detectCores() , N)) 
        print(paste("Parallel: cores ", core))
        cl <- parallel::makeForkCluster(core)
        doParallel::registerDoParallel(cl)
        
        gamma <- foreach(m = 1:N, .combine = 'c') %dopar% {
          set.seed(m+n_labled*100+n_unlabled*10000+alpha*100000)
          sample <- sampler_NB(n_labled,n_unlabled,data, formula)
          train <- sample[[1]]
          unlabeld <- sample[[2]]
          test <-sample[[3]]
          
          e_admissible <- e_admissible_SSL(train, unlabeld, test, alpha)
          SSL <- refernce_SSL(train, unlabeld, test, priori = NULL)
          SL <- refernce_SL(train, unlabeld, test, priori = NULL) 
          
          list(e_admissible=e_admissible,SSL = SSL, SL= SL )
        }
        
        path = paste(getwd(),"/NaiveBayes/results_NB/", dat,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), sep="")
        save(gamma, file = path)
        parallel::stopCluster(cl)
        
        print(paste("DONE: n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
      }
    }
    
  }
}
