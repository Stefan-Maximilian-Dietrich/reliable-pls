while(TRUE){
  #Geschützer Bereich
  wait()
  experiemnt <- get_experiment()
  change_semaphor(TRUE)
  ####
  
  
  dat <- experiemnt$data
  n_labled <- experiemnt$L
  n_unlabled <- experiemnt$U
  alpha <-  experiemnt$alp
  priori_tec <-  experiemnt$prio_t
  refinement <-experiemnt$prio_r
  n_hidden <- experiemnt$n_hidden
  data_loader(dat)
  
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  
  #### NN-Achitecture 
  d <- ncol(data) - 1
  h <- n_hidden 
  K <- length(levels_present)
  n_param <-  h*d + h + K*h + K
  classes <- unique(data$target)
  
  
  priors <- gerate_normal_priori(n_param = n_param, refinement) # n_param abhänig von der netz struktur 
  
  n_prioris <-  length(priors)
  print(paste("RUNNING on ",workers, " workers: data: ", dat, " hidden: ",h," n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, " prioris: ",n_prioris , sep= "")) 
  time_a <- Sys.time()
  
  # 2. Parallelisierungsstrategie festlegen (plattformunabhängig)
  plan(multisession, workers = workers)
  #plan(sequential)
  
  
  # 3. Fortschrittsbalken aktivieren
  handlers("txtprogressbar")  # andere möglich: "cli", "rstudio", "progress"
  
  # 4. Verarbeitung mit Fortschrittsanzeige
  with_progress({
    p <- progressor(steps = N)  # Fortschritt explizit setzen
    
    future_map(1:N, function(i) {
      set.seed(i+n_labled*100+n_unlabled*10000)
      sample <- sampleNN(data, formula, n_labled, n_unlabled)
      train_scaled <- sample[[1]]
      unlabeled_scaled <- sample[[2]]
      test_scaled <-sample[[3]]
      
      if(methods$e_admissible){
        e_admissible <- e_admissible_SSL(priors, train_scaled, unlabeled_scaled, test_scaled, alpha)
        
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h ,"_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/e_admissible/", "ID_", i)
        save(e_admissible, file = titel)
      } 
      
      if(methods$maximal){
        maximal <- maximal_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/maximal/", "ID_", i)
        save(maximal, file = titel)
      } 
      
      if(methods$SSL){
        SSL <-refernce_SSL(train_scaled, unlabeled_scaled, test_scaled)
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL/", "ID_", i)
        save(SSL, file = titel)
      } 
      
      if(methods$SL){
        SL <- refernce_SL(train_scaled, unlabeled_scaled, test_scaled) 
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SL/", "ID_", i)
        save(SL, file = titel)
      }
      
      if(methods$SSL_variance){
        SSL_variance <- refernce_SSL_variance(train, unlabeld, test, priori = NULL) 
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL_variance/", "ID_", i)
        save(SSL_variance, file = titel)
      } 
      
      if(methods$SSL_entropy){
        SSL_entropy <- refernce_SSL_entropy(train, unlabeld, test, priori = NULL) 
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL_entropy/", "ID_", i)
        save(SSL_entropy, file = titel)
      } 
      
      if(methods$M_MaxiMin){
        M_MaxiMin <- M_MaxiMin_SSL(priors, train_scaled, unlabeled_scaled, test_scaled, alpha)
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data,  "_H_", h ,"_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/M_MaxiMin/", "ID_", i)
        save(M_MaxiMin, file = titel)
      } 
      
      if(methods$M_MaxiMax){
        M_MaxiMax <- M_MaxiMax_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/M_MaxiMax/", "ID_", i)
        save(M_MaxiMax, file = titel)
      } 
      
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
    })
  })
  
  
  #### Plot 
  #Graphic_on_the_fly(path)
  #Geschützer Bereich
  pa <-  paste0(save_path, "MASTER/results/",results_dir,"/" ,experiemnt$data, "_H_", h , "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r)
  make_Result_Graph(pa)
  
  
  wait()
  experiemnt <- get_experiment()
  change_semaphor(TRUE)
  ####
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  
}
