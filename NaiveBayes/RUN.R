### setup Session 
source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))

### run Experiments
workers = 4
N = 4
methods <- list(SL = T, 
                SSL = T,
                e_admissible= T, 
                maximal = T, 
                Gamma_MaxiMin = T, 
                Gamma_MaxiMax = T,
                M_MaxiMin = T, 
                M_MaxiMax = T)
# Methodee 1 (Zielstrukur)
Experiments <- list( # All Experiments are define direct
  list(data = "Banknote", L = 6, U = 20, alp = 0.9, prio_t = "grid", prio_r = 20),
  list(data = "Banknote", L = 6, U = 40, alp = 0.9, prio_t = "grid", prio_r = 20),
  list(data = "Banknote", L = 6, U = 60, alp = 0.9, prio_t = "grid", prio_r = 20)
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
Experiments <- paths_to_experiment() 

suffix <- "" #
source(paste(getwd(),"/NaiveBayes/run_benchmark_nb.R", sep = ""))

### analyse Results
online <- FALSE #Wenn TRUE anaylse dirket im LRZ Cluster 
make_all_Graphics(online) 
show_Summary(online)

create_full_match_matrices(Results_end(online)[, -c(1:4)])
View(Results_end(online)) #Vergleicht accuracy der Finalen Modelle 
View(Results_best(online)) #Vergleicht accuracy der besten Modelle 