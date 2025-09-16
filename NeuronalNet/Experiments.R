NeuronalNet <- data.frame(data = c("Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris","Iris" ),
                  L = c(6, 6, 6, 12, 12, 12, 18, 18, 24, 24),
                  U = c(10, 40, 80, 10, 40, 80, 40, 80, 40, 80),
                  alpha = c(0.5,0.8,0.5,0.8,0.5,0.8,0.5,0.8,0.5,0.8),
                  prio_t =c("random", "random","random","random","random","random","random","random","random","random"),
                  prio_r = c(5, 10, 50, 5, 10, 50, 5, 10 ,50, 100),
                  inProgress = F,
                  overall = F,
                  SL = F, 
                  SSL = F,
                  e_admissible= F
)
save(NeuronalNet, file = "/Users/Stefan/Desktop/NeuronalNet")
