NeuronalNet <- data.frame(data = c("Iris", "Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris" ),
                          L = c(3, 6, 6, 6, 12, 12, 12, 18, 18, 24, 24),
                          U = c(5, 10, 40, 80, 10, 40, 80, 40, 80, 40, 80),
                          alpha = c(0.5, 0.5,0.8,0.5,0.8,0.5,0.8,0.5,0.8,0.5,0.8),
                          prio_t =c("random", "random", "random","random","random","random","random","random","random","random","random"),
                          prio_r = c(5, 5, 10, 50, 5, 10, 50, 5, 10 ,50, 100),
                          inProgress = F,
                          overall = F,
                          SL = F, 
                          SSL = F,
                          e_admissible= F
)

NeuronalNet_test <- NeuronalNet[c(1,2), ]

#RESET EXPERIMENTS
save(NeuronalNet, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet")

#RESET TEST
save(NeuronalNet, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet_test")

# ADD all Directorys
update_directory_strucutre(dir = NeuronalNet_test)


mat <- matrix(nrow =11, ncol = 0)

for(k in 1:60) {
  vec <- c()
  for(j in 1:11) {
    adress <- paste0("/Users/Stefan/Desktop/SL/ID_", k)
    load(adress)
    vec <- c(vec, SL[[j]]$overall[1])
  }
  mat <- cbind(mat, vec)
}

plot(rowMeans(mat))