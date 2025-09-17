### setup Session 
source(paste(getwd(),"/NeuronalNet/setup_NN_session.R", sep = ""))

save_path <- "/dss/dsshome1/03/di35lox/"
### run Experiments
workers = 20
N = 60

#Architecture
K <- 3 #outbut (classes)
h <- 5 #hidden
d <- 4 #imput

methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                SSL_variance = F,
                SSL_entropy = F,
                maximal = F, 
                M_MaxiMin = F, 
                M_MaxiMax = F)

source(paste(getwd(),"/NeuronalNet/run_meta.R", sep = ""))   

########### Analyse
check_done()
make_all_Graphics_new()
