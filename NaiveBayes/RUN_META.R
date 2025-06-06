### setup Session 
source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))

save_path <- "/dss/dsshome1/03/di35lox/"
### run Experiments
workers = 20
N = 60
methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                SSL_variance = T,
                SSL_entropy = T,
                maximal = T, 
                M_MaxiMin = T, 
                M_MaxiMax = T)

source(paste(getwd(),"/NaiveBayes/META_run_nb.R", sep = ""))   

########### Analyse
check_done()
make_all_Graphics_new()
