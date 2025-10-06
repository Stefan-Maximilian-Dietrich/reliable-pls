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



QDA_Mixed  <- data.frame(data = c("Cassini", "Cassini","Cassini","Cassini","Circle","Circle","Circle","Circle","Diabetes","Diabetes","Diabetes","Diabetes", "Iris", "Iris","Iris","Iris","Penguins","Penguins","Penguins","Penguins","Seeds","Seeds","Seeds","Seeds" ),
                        L = c(6, 12, 24, 96, 4, 8, 16, 64, 4, 8, 16, 32,6, 12, 24, 48, 6, 12, 24, 48, 6, 12, 24, 48),
                        U = c(96, 96, 96, 96, 64, 64, 64, 64, 64, 64, 64, 64,48, 48, 48, 48,96, 96, 96, 96, 96, 96, 96, 96),
                        alpha = c(0.1),
                        prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                        prio_r = c(200),
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


QDA_Penguins <- data.frame(data = "Penguins" ,
                          L = c(6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12, 6, 6, 6, 6, 6, 6, 12, 12, 12, 12 ,12 ,12),
                          U = c(96, 96, 96, 192, 192, 192, 96, 96, 96, 192, 192, 192, 96, 96, 96, 192, 192, 192, 96, 96, 96, 192, 192, 192),
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

load("/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")

#Greate Experiments Lokal 
save(QDA, file = "/Users/Stefan/Soft_Revision/QDA/QDA") 
#Greate Experiments Global 
save(QDA, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/QDA")

# ADD all Directorys
update_directory_strucutre()

View(QDA)
QDA[QDA$data == "Cassini", ] <- NULL
QDA[QDA$data == "circle", ] <- NULL

QDA <- QDA[QDA$data != "circle", ]




### Add axperiments 
add_experimetns(QDA_Mixed)
update_directory_strucutre()

