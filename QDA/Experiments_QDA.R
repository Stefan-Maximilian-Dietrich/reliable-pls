QDA_Bank <- data.frame(data = c("Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote", "Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote" ),
                       L = c(4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8),
                       U = c(64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128),
                       alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5, 0.00003, 0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003, 0.00003, 0.00003),
                       prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                       prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                       inProgress = F,
                       SL = F, 
                       SSL = F,
                       e_admissible= F, 
                       SSL_variance = F,
                       SSL_entropy = F,
                       maximal = F, 
                       M_MaxiMin = F, 
                       M_MaxiMax = F
)

QDA_Cars <- data.frame(data = c("cars", "cars","cars","cars","cars","cars","cars","cars","cars","cars","cars","cars", "cars", "cars","cars","cars","cars","cars","cars","cars","cars","cars","cars","cars" ),
                       L = c(4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8),
                       U = c(8, 8, 8, 16, 16, 16, 8, 8, 8, 16, 16, 16, 8, 8, 8, 16, 16, 16, 8, 8, 8, 16, 16, 16),
                       alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5, 0.00003, 0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003, 0.00003, 0.00003),
                       prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                       prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                       inProgress = F,
                       SL = F, 
                       SSL = F,
                       e_admissible= F, 
                       SSL_variance = F,
                       SSL_entropy = F,
                       maximal = F, 
                       M_MaxiMin = F, 
                       M_MaxiMax = F
)



QDA_Bank2 <- data.frame(data = c("Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote", "Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote" ),
                        L = c(4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8),
                        U = c(64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128),
                        alpha = c(0.9, 0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9, 0.9, 0.9, 0.0000003, 0.0000003,0.0000003,0.0000003,0.0000003,0.0000003,0.0000003,0.0000003,0.0000003,0.0000003, 0.0000003, 0.0000003),
                        prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                        prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                        inProgress = F,
                        SL = F, 
                        SSL = F,
                        e_admissible= F, 
                        SSL_variance = F,
                        SSL_entropy = F,
                        maximal = F, 
                        M_MaxiMin = F, 
                        M_MaxiMax = F
)


QDA_Cassini <- data.frame(data = "Cassini" ,
                          L = c(6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12, 6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12),
                          U = c(256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128),
                          alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5, 0.00003, 0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003, 0.00003, 0.00003),
                          prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                          prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                          inProgress = F,
                          SL = F, 
                          SSL = F,
                          e_admissible= F, 
                          SSL_variance = F,
                          SSL_entropy = F,
                          maximal = F, 
                          M_MaxiMin = F, 
                          M_MaxiMax = F
)
QDA_Circle <- data.frame(data = "Circle" ,
                         L = c(6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12, 6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12),
                         U = c(256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128, 256, 256, 256, 128, 128, 128),
                         alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5, 0.00003, 0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003, 0.00003, 0.00003),
                         prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                         prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                         inProgress = F,
                         SL = F, 
                         SSL = F,
                         e_admissible= F, 
                         SSL_variance = F,
                         SSL_entropy = F,
                         maximal = F, 
                         M_MaxiMin = F, 
                         M_MaxiMax = F
)


ground_path = "/dss/dsshome1/03/di35lox/MASTER/results/QDA"

#Greate Experiments Lokal 
save(QDA, file = "/Users/Stefan/Soft_Revision/QDA/QDA") 
#Greate Experiments Global 
save(QDA, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")

# ADD all Directorys
update_directory_strucutre()

### Add axperiments 
add_experimetns(QDA_Circle)

