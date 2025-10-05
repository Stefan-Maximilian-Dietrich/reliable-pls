QDA_Bank <- data.frame(data = c("Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote", "Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote" ),
                       L = c(4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8, 4, 4, 4, 4, 4, 4, 8, 8, 8, 8 ,8 ,8),
                       U = c(64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128, 64, 64, 64, 128, 128, 128),
                       alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5, 0.00003, 0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003,0.00003, 0.00003, 0.00003),
                       prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                       prio_r = c(10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000, 10,  100, 1000, 10, 100, 1000, 10, 100, 1000 ,10, 100, 1000),
                       inProgress = F,
                       overall = F,
                       SL = F, 
                       SSL = F,
                       e_admissible= F, 
                       SSL_variance = F,
                       SSL_entropy = F,
                       maximal = F, 
                       M_MaxiMin = F, 
                       M_MaxiMax = F
)

QDA_Cassini <- data.frame(data = c("Cassini", "Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini" ),
                          L = c(6, 6, 6, 6, 6, 6, 6, 6, 12, 12 ,12 ,12),
                          U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                          alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                          prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                          prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                          inProgress = F,
                          overall = F,
                          SL = F, 
                          SSL = F,
                          e_admissible= F, 
                          SSL_variance = F,
                          SSL_entropy = F,
                          maximal = F, 
                          M_MaxiMin = F, 
                          M_MaxiMax = F
)

QDA_Wine <- data.frame(data = c("Wine", "Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine" ),
                       L = c(6, 6, 6, 6, 6, 6, 6, 6, 12, 12 ,12 ,12),
                       U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                       alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                       prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                       prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                       inProgress = F,
                       overall = F,
                       SL = F, 
                       SSL = F,
                       e_admissible= F, 
                       SSL_variance = F,
                       SSL_entropy = F,
                       maximal = F, 
                       M_MaxiMin = F, 
                       M_MaxiMax = F
)

QDA_Breast_Cancer <- data.frame(data = c("Breast_Cancer", "Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer" ),
                                L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                                U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                                alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                                prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                                prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                                inProgress = F,
                                overall = F,
                                SL = F, 
                                SSL = F,
                                e_admissible= F, 
                                SSL_variance = F,
                                SSL_entropy = F,
                                maximal = F, 
                                M_MaxiMin = F, 
                                M_MaxiMax = F
)


QDA_Seeds <- data.frame(data = c("Seeds", "Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds" ),
                        L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                        U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                        alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                        prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                        prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                        inProgress = F,
                        overall = F,
                        SL = F, 
                        SSL = F,
                        e_admissible= F, 
                        SSL_variance = F,
                        SSL_entropy = F,
                        maximal = F, 
                        M_MaxiMin = F, 
                        M_MaxiMax = F
                        
)

QDA <- rbind(QDA_Bank) #, QDA_Breast_Cancer,QDA_Cassini, QDA_Wine )

#Greate Experiments Lokal 
save(QDA, file = "/Users/Stefan/Soft_Revision/QDA/QDA") 
#Greate Experiments Global 
save(QDA, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")

# ADD all Directorys
update_directory_strucutre()


A <- unlist(lapply(priors, function(X) det(X$Lambda0))) 
