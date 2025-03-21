data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

name_df = "banknote" # for results 
data = "banknote"
# formula for glm
formula = target ~  Diagonal+ Bottom + Length #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())

data = data_frame
formula = target ~ Diagonal + Bottom + Length

N = 60
for(n_labled in c(4,6,8,10,12)) { 
  for(n_unlabled in c(2,4,8,16,32,64,128)) {
    for(alpha in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) {
      core <- as.numeric(min(20, parallel::detectCores() , N)) #
      print(paste("Parallel: cores ", core))
      cl <- parallel::makeForkCluster(core)
      doParallel::registerDoParallel(cl)
      
      gamma <- foreach(iter = 1:N, .combine = 'c') %dopar% {
        set.seed(iter)
        source("NB.R")
      }
      path = paste(getwd(),"/NB_eadmissible/", "experiment_","n_labled_", as.character(n_labled), "_n_unlabled_",  as.character(n_unlabled), "_alpha_" , as.character(alpha), sep="")
      save(gamma, file = path)
      print(paste("DONE: n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
    }
  }
  
}

