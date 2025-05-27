### setup Session 
source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))

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

source(paste(getwd(),"/NaiveBayes/run_benchmark_nb.R", sep = ""))   

