source(paste(getwd(),"/NaiveBayes/_setup_NB_session.R", sep = ""))
N = 60
workers = 20



experiemnts <- all_experiments(online)
experiemnts$refinement <- NA
experiemnts[experiemnts$data == "banknote",]$refinement <- 100
experiemnts[experiemnts$data == "ionosphere",]$refinement <- 100
experiemnts[experiemnts$data == "brestAll",]$refinement <- 100
experiemnts[experiemnts$data == "iris",]$refinement <- 20
experiemnts[experiemnts$data == "waveform",]$refinement <- 20
experiemnts[experiemnts$data == "simulatedA",]$refinement <- 10




## all need a step sice

# delete for neasted for loops in favour of one for loop tha does all axperiments 


for(i in 13:15){
  
  dat <- experiemnts[i,]$data
  n_labled <- experiemnts[i,]$L
  n_unlabled <- experiemnts[i,]$U
  alpha <-  experiemnts[i,]$alp
  refinement <-experiemnts[i,]$refinement
  
  
  data_loader(dat)
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  prioris <- gerate_priori_simplex_rec(levels_present, refinement)
  
  print(paste("RUNNING on ",workers, " workers: data ", dat, " n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= "")) ### könnte die anzahl der prioris hinzunehmen 
  time_a <- Sys.time()
  
  # 2. Parallelisierungsstrategie festlegen (plattformunabhängig)
  plan(multisession, workers = workers)
  
  # 3. Fortschrittsbalken aktivieren
  handlers("txtprogressbar")  # andere möglich: "cli", "rstudio", "progress"
  
  # 4. Verarbeitung mit Fortschrittsanzeige
  with_progress({
    p <- progressor(steps = N)  # Fortschritt explizit setzen
    
    gamma_l <- future_map(1:N, function(i) {
      set.seed(i+n_labled*100+n_unlabled*10000+alpha*100000)
      
      sample <- sampler_NB_up(n_labled,n_unlabled,data, formula)
      train <- sample[[1]]
      unlabeld <- sample[[2]]
      test <-sample[[3]]
      
      e_admissible <- e_admissible_SSL(prioris, train, unlabeld, test, alpha)
      maximal <- maximal_SSL(prioris, train, unlabeld, test, alpha)
      Gamma_MaxiMin <- Gamma_MaxiMin_SSL(prioris, train, unlabeld, test, alpha)
      Gamma_MaxiMax <- Gamma_MaxiMax_SSL(prioris, train, unlabeld, test, alpha)
      SSL <- refernce_SSL(train, unlabeld, test, priori = NULL)
      SL <- refernce_SL(train, unlabeld, test, priori = NULL) 
      
      l <- list(e_admissible=e_admissible,SSL = SSL, SL= SL, Gamma_MaxiMin = Gamma_MaxiMin, Gamma_MaxiMax=Gamma_MaxiMax, maximal = maximal )
      
      # Fortschritt updaten
      p(sprintf("Zeile %d bearbeitet", i))
      
      return(l)
    })
  })
  gamma <- unlist(gamma_l, recursive = FALSE)
  
  
  path = paste(getwd(),"/NaiveBayes/results_NB/", dat,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), sep="")
  save(gamma, file = path)
  
  #### Plot 
  Graphic_on_the_fly(path)
  
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  
  
  
  
  
}
