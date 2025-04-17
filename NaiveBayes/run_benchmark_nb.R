#load Functions
source(paste(getwd(),"/NaiveBayes/Function_NB.R", sep = ""))
source(paste(getwd(),"/NaiveBayes/Methods.R", sep = ""))


label <- c(6)
unlabel <- c(20)
alp <- c(0.9)
N = 4
used_data <- c("brestAll")

for(dat in used_data) {
  data_loader(dat)
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  prioris <- generate_priori_simplex(levels_present, step = 0.1)
  for(n_labled in label) { 
    for(n_unlabled in unlabel) {
      for(alpha in alp) {
        print(paste("RUNNING: data ", dat, " n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
        core <- as.numeric(min(20, parallel::detectCores() , N)) 
        print(paste("Parallel: cores ", core))
        
        cl <- parallel::makeForkCluster(core)
        doParallel::registerDoParallel(cl)
        
        gamma <- foreach(m = 1:N, .combine = 'c') %dopar% {
          set.seed(m+n_labled*100+n_unlabled*10000)
          set.seed(m+n_labled*100+n_unlabled*10000+alpha*100000)
          
          sample <- sampler_NB_up(n_labled,n_unlabled,data, formula)
          train <- sample[[1]]
          unlabeld <- sample[[2]]
          test <-sample[[3]]
        
          e_admissible <- e_admissible_SSL(prioris, train, unlabeld, test, alpha)
        
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





################################################################################
n_labled <- c(20)
n_unlabled <- c(120)
alpha <- c(0.8)
dat <- c("banknote")
m = 1

n_labled <- c(20)
n_unlabled <- c(120)
alpha <- c(0.99)
dat <- c("banknote")
m = 1

n_labled <- c(20)
n_unlabled <- c(120)
alpha <- c(0.8)
dat <- c("banknote")
m = 2

#
n_labled <- c(20)
n_unlabled <- c(170)
alpha <- c(0.8)
dat <- c("banknote")
m = 2
## gut 

n_labled <- c(20)
n_unlabled <- c(120)
alpha <- c(0.8)
dat <- c("iris")
m = 2
## iris 

n_labled <- c(20)
n_unlabled <- c(170)
alpha <- c(0.9)
dat <- c("banknote")
m = 2
##schecht aber zeigt an das zu viel alpha schlecht ist 

n_labled <- c(20)
n_unlabled <- c(100)
alpha <- c(0.)
dat <- c("banknote")
m = 2 ## großer richtiger bogen