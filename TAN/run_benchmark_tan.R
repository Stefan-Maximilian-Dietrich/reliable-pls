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
  
  prioris <- as.data.table(gerate_priori_simplex_rec(levels_present, refinement)) # muss neu gemacht werden 
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
      prioris <- as.data.table(gerate_priori_simplex_rec(levels_present, refinement)) # muss neu gemacht werden 
      data_samp <- sampler_NB_up(n_labled,n_unlabled,data, formula)
      dt_train <- as.data.table(discretize_rec_min_ent(data_samp[[1]]))
      dt_test <- as.data.table(discretize_rec_min_ent_test(data_samp[[1]], data_samp[[3]]))
      dt_unlabled <- as.data.table(discretize_rec_min_ent_test(data_samp[[1]], data_samp[[2]]))
      
      if(structure == "TAN") {
        if(methods$e_admissible){
          e_admissible <- tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {e_admissible <- NA}
        
        if(methods$maximal){
          maximal <-tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {maximal <- NA}
        
        if(methods$SSL){
          SSL <-tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {SSL <- NA}
        
        if(methods$SL){
          SL <- tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {SL <- NA}
        
        if(methods$SSL_variance){
          SSL_variance <- tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {SSL_variance <- NA}
        
        if(methods$SSL_entropy){
          SSL_entropy <- tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {SSL_entropy <- NA}
        
        if(methods$M_MaxiMin){
          M_MaxiMin <-tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {M_MaxiMin <- NA}
        
        if(methods$M_MaxiMax){
          M_MaxiMax <- tan_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha, criterion = criterion)

        } else {M_MaxiMax <- NA}
        
      }
      if(structure == "NB") {
        if(methods$e_admissible){
          e_admissible <- nb_SSL_E_admissible(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {e_admissible <- NA}
        
        if(methods$maximal){
          maximal <-nb_SSL_maximal(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {maximal <- NA}
        
        if(methods$SSL){
          SSL <-nb_SSL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {SSL <- NA}
        
        if(methods$SL){
          SL <- nb_SL(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {SL <- NA}
        
        if(methods$SSL_variance){
          SSL_variance <- nb_SSL_variance(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {SSL_variance <- NA}
        
        if(methods$SSL_entropy){
          SSL_entropy <-nb_SSL_entropy(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {SSL_entropy <- NA}
        
        if(methods$M_MaxiMin){
          M_MaxiMin <-nb_SSL_M_MaxiMin(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {M_MaxiMin <- NA}
        
        if(methods$M_MaxiMax){
          M_MaxiMax <- nb_SSL_M_MaxiMax(prioris = prioris, train = dt_train, unlabeld = dt_unlabled, test = dt_test, alpha = alpha)
          
        } else {M_MaxiMax <- NA}
        
      }
      

     
      l <- list(e_admissible=e_admissible,SSL = SSL, SL= SL, SSL_variance = SSL_variance,SSL_entropy = SSL_entropy,  maximal = maximal, M_MaxiMin= M_MaxiMin, M_MaxiMax = M_MaxiMax)
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
      return(l)
    })
  })
  gamma <- unlist(gamma_l, recursive = FALSE)
  
  
  path = paste(getwd(),"/TAN/results_TAN/", dat,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), "_prio_",as.character(priori_tec), "_", as.character(refinement), as.character(suffix) , sep="")
  save(gamma, file = path)
  
  #### Plot  
  Graphic_on_the_fly(path)
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  
}

