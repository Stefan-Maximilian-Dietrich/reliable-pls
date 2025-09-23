### setup Session 
source(paste(getwd(),"/NeuronalNet/setup_NN_session.R", sep = ""))

save_path <- "/dss/dsshome1/03/di35lox/"
results_dir <- "NeuronalNet"
### run Experiments
workers = 20
N = 60

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

########### Analyse
check_done()
make_all_Graphics_new()
