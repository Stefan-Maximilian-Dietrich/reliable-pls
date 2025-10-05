source(paste(getwd(),"/QDA/setup_QDA.R", sep = ""))



experiments_path <- "/Users/Stefan/Soft_Revision/QDA/QDA" #Local
experiments_path <- "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA" #LRZ

save_path <- "/Users/Stefan/Soft_Revision/QDA/results/" #Local
save_path <- "/dss/dsshome1/03/di35lox/MASTER/results/QDA/" #LRZ

use_parallel   <- TRUE     # TRUE = parallel (multisession), FALSE = sequentiell
error_log_file <- NULL     # z.B. "error_log.txt" setzen, um Fehler in Datei zu loggen
workers = 20
N = 100

#Architecture 

methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                SSL_variance = T,
                SSL_entropy = T,
                maximal = T, 
                M_MaxiMin = T, 
                M_MaxiMax = T) 

source(paste(getwd(),"/QDA/Execution_QDA.R", sep = ""))   