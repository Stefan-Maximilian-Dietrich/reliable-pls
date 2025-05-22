for(j in 1:length(Experiments)){
  
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
    
    gamma_l <- future_map(1:N, function(i) {
      set.seed(i+n_labled*100+n_unlabled*10000)
      
      sample <- sampler_NB_up(n_labled,n_unlabled,data, formula)
      train <- sample[[1]]
      unlabeld <- sample[[2]]
      test <-sample[[3]]
      
      if(methods$e_admissible){
        e_admissible <- e_admissible_SSL(prioris, train, unlabeld, test, alpha)
      } else {e_admissible <- NA}
      
      if(methods$maximal){
        maximal <- maximal_SSL(prioris, train, unlabeld, test, alpha)
      } else {maximal <- NA}
      
      if(methods$SSL){
        SSL <- refernce_SSL(train, unlabeld, test, priori = NULL)
      } else {SSL <- NA}
      
      if(methods$SL){
        SL <- refernce_SL(train, unlabeld, test, priori = NULL) 
      } else {SL <- NA}
      
      if(methods$SSL_variance){
        SSL_variance <- refernce_SSL_variance(train, unlabeld, test, priori = NULL) 
      } else {SSL_variance <- NA}
      
      if(methods$SSL_entropy){
        SSL_entropy <- refernce_SSL_entropy(train, unlabeld, test, priori = NULL) 
      } else {SSL_entropy <- NA}
      
      if(methods$M_MaxiMin){
        M_MaxiMin <- M_MaxiMin_SSL(prioris, train, unlabeld, test, alpha)
      } else {M_MaxiMin <- NA}
      
      if(methods$M_MaxiMax){
        M_MaxiMax <- M_MaxiMax_SSL(prioris, train, unlabeld, test, alpha)
      } else {M_MaxiMax <- NA}
      
      l <- list(e_admissible=e_admissible,SSL = SSL, SL= SL, SSL_variance = SSL_variance,SSL_entropy = SSL_entropy,  maximal = maximal, M_MaxiMin= M_MaxiMin, M_MaxiMax = M_MaxiMax)
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
      return(l)
    })
  })
  gamma <- unlist(gamma_l, recursive = FALSE)
  
  
  path = paste(getwd(),"/NaiveBayes/results_NB/", dat,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), "_prio_",as.character(priori_tec), "_", as.character(refinement), as.character(suffix) , sep="")
  save(gamma, file = path)
  
  #### Plot 
  Graphic_on_the_fly(path)
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  
}

