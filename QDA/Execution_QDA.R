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
  
  data_loader(dat)
  
  
  print(paste("RUNNING on ",workers, " workers: data ", dat, " n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, " prioris: ",refinement , sep= "")) 
  time_a <- Sys.time()
  
  # 2. Parallelisierungsstrategie festlegen (plattformunabhängig)
  plan(multisession, workers = workers)
  
  # 3. Fortschrittsbalken aktivieren
  handlers("txtprogressbar")  # andere möglich: "cli", "rstudio", "progress"
  
  # 4. Verarbeitung mit Fortschrittsanzeige
  with_progress({
    p <- progressor(steps = N)  # Fortschritt explizit setzen
    
    future_map(1:N, function(i) {
      set.seed(i)
      sample <- sampler_NB_up(n_labled,n_unlabled,data, formula)
      data_train <- sample[[1]]
      data_unlabeled <- sample[[2]]
      data_test <-sample[[3]]
      
      priors <-  generate_random_priors(data_train, n_priors = refinement,  kappa_range = c(0.1, 3), scale_range = c(0.5, 3),  nu_extra = c(0, 5, 10))      
      
      if(methods$e_admissible){
        #print(paste0(i," Start: e_admissible"))
        e_admissible <- e_admissible_SSL(priors, data_train, data_unlabeled, data_test, alpha)
        titel <- paste0(save_path ,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/e_admissible/", "ID_", i)
        save(e_admissible, file = titel)
        #print(paste0(i," DONE: e_admissible"))
      } 
      
      if(methods$maximal){
        #print(paste0(i," Start: maximal"))
        maximal <- maximal_SSL(priors, data_train, data_unlabeled, data_test, alpha)
        titel <- paste0(save_path ,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/maximal/", "ID_", i)
        save(maximal, file = titel)
        #print(paste0(i," DONE: maximal"))
        
      } 
      
      if(methods$SSL){
        #print(paste0(i," Start: SSL"))
        
        SSL <- refernce_SSL(data_train, data_unlabeled, data_test)
        titel <- paste0(save_path,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL/", "ID_", i)
        save(SSL, file = titel)
        #print(paste0(i," DONE: SSL"))
        
      } 
      
      if(methods$SL){
        #print(paste0(i," Start: SL"))
        
        SL <- refernce_SL(data_train, data_unlabeled, data_test)
        titel <- paste0(save_path,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SL/", "ID_", i)
        save(SL, file = titel)
        #print(paste0(i," DONE: SL"))
        
      }
      
      if(methods$SSL_variance){
        #print(paste0(i," Start: SSL_variance"))
        
        SSL_variance <- refernce_SSL_variance(data_train, data_unlabeled, data_test)
        titel <- paste0(save_path ,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL_variance/", "ID_", i)
        save(SSL_variance, file = titel)
        #print(paste0(i," DONE: SSL_variance"))
        
      } 
      
      if(methods$SSL_entropy){
        #print(paste0(i," Start: SSL_entropy"))
        
        SSL_entropy <- refernce_SSL_entropy(data_train, data_unlabeled, data_test)
        titel <- paste0(save_path ,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/SSL_entropy/", "ID_", i)
        save(SSL_entropy, file = titel)
        #print(paste0(i," DONE: SSL_entropy"))
        
      } 
      
      if(methods$M_MaxiMin){
        #print(paste0(i," Start: M_MaxiMin"))
        
        M_MaxiMin <- M_MaxiMin_SSL(priors, data_train, data_unlabeled, data_test, alpha)
        titel <- paste0(save_path,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/M_MaxiMin/", "ID_", i)
        save(M_MaxiMin, file = titel)
        #print(paste0(i," DONE: M_MaxiMin"))
        
      } 
      
      if(methods$M_MaxiMax){
        #print(paste0(i," Start: M_MaxiMax"))
        
        M_MaxiMax <- M_MaxiMax_SSL(priors, data_train, data_unlabeled, data_test, alpha)
        titel <- paste0(save_path,experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r, "/M_MaxiMax/", "ID_", i)
        save(M_MaxiMax, file = titel)
        #print(paste0(i," DONE: M_MaxiMax"))
        
      } 
      
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
    })
  })
  
  
  #### Plot 
  
  experiment_path <-  paste0("/dss/dsshome1/03/di35lox/MASTER/results/QDA/", experiemnt$data, "_L_", experiemnt$L, "_U_",  experiemnt$U, "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r)
  make_Result_Graph(experiment_path)
  #Geschützer Bereich
  wait()
  experiemnt <- get_experiment()
  change_semaphor(TRUE)
  ####
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  try(future:::ClusterRegistry("stop"), silent = TRUE) # Falls session hängt 
  
  
}