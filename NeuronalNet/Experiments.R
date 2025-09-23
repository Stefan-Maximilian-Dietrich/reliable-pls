NeuronalNet_Bank <- data.frame(data = c("Banknote", "Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote","Banknote" ),
                          L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                          U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                          alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                          prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                          prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                          n_hidden = c(1,  2, 3, 1, 2, 3, 1, 2, 3 ,1, 2,3),
                          inProgress = F,
                          overall = F,
                          SL = F, 
                          SSL = F,
                          M_MaxiMin = F,
                          e_admissible= F
)

NeuronalNet_Cassini <- data.frame(data = c("Cassini", "Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini","Cassini" ),
                               L = c(3, 3, 3, 3, 6, 6, 6, 6, 12, 12 ,12 ,12),
                               U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                               alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                               prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                               prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                               n_hidden = c(2,  3, 4, 2, 3, 4, 2, 3, 4 ,2, 3,4),
                               inProgress = F,
                               overall = F,
                               SL = F, 
                               SSL = F,
                               M_MaxiMin = F,
                               e_admissible= F
)

NeuronalNet_Wine <- data.frame(data = c("Wine", "Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine","Wine" ),
                               L = c(3, 3, 3, 3, 6, 6, 6, 6, 12, 12 ,12 ,12),
                               U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                               alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                               prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                               prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                               n_hidden = c(2,  3, 4, 2, 3, 4, 2, 3, 4 ,2, 3,4),
                               inProgress = F,
                               overall = F,
                               SL = F, 
                               SSL = F,
                               M_MaxiMin = F,
                               e_admissible= F
)

NeuronalNet_Breast_Cancer <- data.frame(data = c("Breast_Cancer", "Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer","Breast_Cancer" ),
                               L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                               U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                               alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                               prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                               prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                               n_hidden = c(1,  2, 3, 1, 2, 3, 1, 2, 3 ,1, 2,3),
                               inProgress = F,
                               overall = F,
                               SL = F, 
                               SSL = F,
                               M_MaxiMin = F,
                               e_admissible= F
)


NeuronalNet_Seeds <- data.frame(data = c("Seeds", "Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds" ),
                               L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                               U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                               alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                               prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                               prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                               n_hidden = c(1,  2, 3, 1, 2, 3, 1, 2, 3 ,1, 2,3),
                               inProgress = F,
                               overall = F,
                               SL = F, 
                               SSL = F,
                               M_MaxiMin = F,
                               e_admissible= F
)

NeuronalNet_Seeds <- data.frame(data = c("Seeds", "Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds","Seeds" ),
                                L = c(2, 2, 2, 2, 4, 4, 4, 4, 8, 8 ,8 ,8),
                                U = c(32, 32, 64, 64, 32, 32, 64, 64, 32, 32, 64, 64),
                                alpha = c(0.5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5, 0.5, 0.5),
                                prio_t =c("random","random", "random", "random","random","random","random","random","random","random", "random", "random"),
                                prio_r = c(60,  60, 60, 60, 60, 60, 60, 60, 60 ,60, 60, 60),
                                n_hidden = c(1,  2, 3, 1, 2, 3, 1, 2, 3 ,1, 2,3),
                                inProgress = F,
                                overall = F,
                                SL = F, 
                                SSL = F,
                                M_MaxiMin = F,
                                e_admissible= F
)

NeuronalNet <- rbind(NeuronalNet_Bank, NeuronalNet_Breast_Cancer,NeuronalNet_Cassini, NeuronalNet_Wine )
###
load(file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet")
NeuronalNet <- rbind(NeuronalNet, NeuronalNet_Seeds)
save(NeuronalNet, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet")

#RESET EXPERIMENTS
save(NeuronalNet, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet")

#RESET TEST
save(NeuronalNet, file = "/dss/dsshome1/03/di35lox/MASTER/experiments/NeuronalNet_test")

# ADD all Directorys
update_directory_strucutre(dir = "NeuronalNet")

make_Result_Graph("/Users/Stefan/Desktop/Breast_Cancer_H_1_L_2_U_64_alp_0.5_random_60")
make_Result_matrix("/Users/Stefan/Desktop/Banknote_H_1_L_2_U_32_alp_0.5_random_60")

ground_path <- "/Users/Stefan/Desktop/NeuronalNet"
graph_path <- "/Users/Stefan/Desktop/Graphs"

make_all_Graphs(ground_path, graph_path) 



make_all_Graphs(ground_path, graph_path)
