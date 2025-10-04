source(paste(getwd(),"/QDA/setup_QDA.R", sep = ""))

experiments_path <- "/Users/Stefan/Soft_Revision/QDA/QDA" #Local
experiments_path <- "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA" #LRZ

save_path <- "/Users/Stefan/Soft_Revision/QDA/results/" #Local
save_path <- "/dss/dsshome1/03/di35lox/MASTER/results/QDA/" #LRZ
  
workers = 20
N = 100

#Architecture

methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                SSL_variance = T,
                SSL_entropy = T,
                maximal = F, 
                M_MaxiMin = T, 
                M_MaxiMax = F)

source(paste(getwd(),"/NeuronalNet/run_meta.R", sep = ""))   