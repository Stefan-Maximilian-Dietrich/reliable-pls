### setup Session 
source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))
save_path <- ""##############################
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
# Methodee 1 (Zielstrukur)
Experiments <- list( # All Experiments are define direct
  list(data = "Seeds", L = 6, U = 22, alp = 0.9, prio_t = "grid", prio_r = 20)
  #,list(data = "Banknote", L = 10, U = 100, alp = 0.9, prio_t = "grid", prio_r = 100),
 # list(data = "Wave", L = 20, U = 60, alp = 0.9, prio_t = "grid", prio_r = 20)
)

# Methode 2
cross_prod <- list( # Experiments are created by cross product 
  data = c("Banknote", "Ionosphere"),
  L = c(8,16),
  U = c(20,40,60),
  alpha = c(0.3,0.9),
  prio_t = c("grid"),
  prio_r = c(10,20)
) 
Experiments <- cross_product_to_experiment(cross_prod) 

# Methode 3  
Experiments <- paths_to_experiment(folder = "/NaiveBayes/results_NB_PCa", select = NULL, met = "SSL_entropy")

suffix <- "_ent" 
source(paste(getwd(),"/NaiveBayes/run_benchmark_nb.R", sep = ""))   

### analyse Results
online <- FALSE #Wenn TRUE anaylse dirket im LRZ Cluster  
make_all_Graphics(online, legende = T) 
make_all_Graphics(online, legende = T, methods = c("SL", "e_admissible", "maximal")) 

Result_end_better_SL(online, 4)
Result_end_better_SSL(online, 2)
create_full_match_matrices(Results_end(online)[, -c(1:5)])
rowSums(create_full_match_matrices(Results_end(online)[, -c(1:5)])[[1]])

View(Results_end(online)) #Vergleicht accuracy der Finalen Modelle 
View(Results_best(online)) #Vergleicht accuracy der besten Modelle 