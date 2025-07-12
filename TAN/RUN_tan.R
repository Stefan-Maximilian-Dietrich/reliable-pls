### setup Session 
source(paste(getwd(),"/TAN/_setup_TAN_session.R", sep = ""))

### run Experiments
workers = 3
N = 25
structure = "NB"
methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                SSL_variance = F,
                SSL_entropy = F,
                maximal = F, 
                M_MaxiMin = F, 
                M_MaxiMax = F)
# Methodee 1 (Zielstrukur)
Experiments <- list( # All Experiments are define direct
  list(data = "Banknote", L = 8, U = 32, alp = 0.9, prio_t = "grid", prio_r = 100),
  list(data = "Wave", L = 20, U = 60, alp = 0.9, prio_t = "grid", prio_r = 20)
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

suffix <- "" 
source(paste(getwd(),"/TAN/run_benchmark_tan.R", sep = ""))   

### analyse Results
online <- FALSE #Wenn TRUE anaylse dirket im LRZ Cluster  
make_all_Graphics(online, legende = T, methods = c("SL", "e_admissible", "maximal")) 
Result_end_better_SL(online, 4)
Result_end_better_SSL(online, 2)
create_full_match_matrices(Results_end(online)[, -c(1:5)])
rowSums(create_full_match_matrices(Results_end(online)[, -c(1:5)])[[1]])

View(Results_end(online)) #Vergleicht accuracy der Finalen Modelle 
View(Results_best(online)) #Vergleicht accuracy der besten Modelle 