CHECK

data_frame(data = rep("Seeds", 10),
           L = c(6,6,6, 12, 12,6,6,6, 12, 12),
           U = c(12,48, 192, 48, 192, 12,48, 192, 48, 192), 
           alp = c(0.9, 0.9,0.9,0.9,0.9,0.3,0.3,0.3,0.3,0.3),
           prio_t = rep("grid", 10),
           prio_r = rep(20, 10),
           inProgress = rep(FALSE, 10),
           overall = rep(FALSE, 10),
           SL = rep(FALSE, 10),
           SSL = rep(FALSE, 10),
           e_admissible = rep(FALSE, 10),
           SSL_variance = rep(FALSE, 10),
           SSL_entropy = rep(FALSE, 10),
           maximal = rep(FALSE, 10),
           M_MaxiMin = rep(FALSE, 10),
           M_MaxiMax = rep(FALSE, 10))