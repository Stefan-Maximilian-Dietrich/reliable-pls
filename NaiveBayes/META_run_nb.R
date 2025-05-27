while(TRUE){
  
  experiemnt <- Experiments[[j]]
  
  dat <- experiemnt$data
  n_labled <- experiemnt$L
  n_unlabled <- experiemnt$U
  alpha <-  experiemnt$alp
  priori_tec <-  experiemnt$prio_t
  refinement <-experiemnt$prio_r
  
  seed <- sum(utf8ToInt(tolower(dat))) # gereiret aus den Wort das die den Datensatz beschriebt 
  set.seed(seed)
  data_loader(dat)
  
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  
  prioris <- gerate_priori_simplex_rec(levels_present, refinement) # muss neu gemacht werden 
  n_prioris <-  nrow(prioris)
  print(paste("RUNNING on ",workers, " workers: data ", dat, " n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, " prioris: ",n_prioris , sep= "")) 
  time_a <- Sys.time()
  
  # 2. Parallelisierungsstrategie festlegen (plattformunabhängig)
  plan(multisession, workers = workers)
  
  # 3. Fortschrittsbalken aktivieren
  handlers("txtprogressbar")  # andere möglich: "cli", "rstudio", "progress"
  
  # 4. Verarbeitung mit Fortschrittsanzeige
  with_progress({
    p <- progressor(steps = N)  # Fortschritt explizit setzen
    
    future_map(1:N, function(i) {
      set.seed(i+n_labled*100+n_unlabled*10000)
      
      sample <- sampler_NB_up(n_labled,n_unlabled,data, formula)
      train <- sample[[1]]
      unlabeld <- sample[[2]]
      test <-sample[[3]]
      
      if(methods$e_admissible){
        e_admissible <- e_admissible_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/e_admissible/", "ID_", i)
        save(e_admissible, file = titel)
      } 
      
      if(methods$maximal){
        maximal <- maximal_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/maximal/", "ID_", i)
        save(maximal, file = titel)
      } 
      
      if(methods$SSL){
        SSL <- refernce_SSL(train, unlabeld, test, priori = NULL)
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/SSL/", "ID_", i)
        save(SSL, file = titel)
      } 
      
      if(methods$SL){
        SL <- refernce_SL(train, unlabeld, test, priori = NULL) 
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/SL/", "ID_", i)
        save(SL, file = titel)
      }
      
      if(methods$SSL_variance){
        SSL_variance <- refernce_SSL_variance(train, unlabeld, test, priori = NULL) 
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/SSL_variance/", "ID_", i)
        save(SSL_variance, file = titel)
      } 
      
      if(methods$SSL_entropy){
        SSL_entropy <- refernce_SSL_entropy(train, unlabeld, test, priori = NULL) 
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/SSL_entropy/", "ID_", i)
        save(SSL_entropy, file = titel)
      } 
      
      if(methods$M_MaxiMin){
        M_MaxiMin <- M_MaxiMin_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/M_MaxiMin/", "ID_", i)
        save(M_MaxiMin, file = titel)
      } 
      
      if(methods$M_MaxiMax){
        M_MaxiMax <- M_MaxiMax_SSL(prioris, train, unlabeld, test, alpha)
        titel <- paste0(save_path, "/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r, "/M_MaxiMax/", "ID_", i)
        save(M_MaxiMax, file = titel)
      } 
      
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
    })
  })
  
  
  #### Plot 
  #Graphic_on_the_fly(path)
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  
}

