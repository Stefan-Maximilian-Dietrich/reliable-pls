#load Functions
source(paste(getwd(),"/NaiveBayes/Function_NB.R", sep = ""))
source(paste(getwd(),"/NaiveBayes/Methods.R", sep = ""))

workers = 4

label <- c(4)
unlabel <- c(4,16)
alp <- c( 0.9)
N = 8
used_data <- c("ionosphere")

for(dat in used_data) {
  data_loader(dat)
  levels_present <- levels(data[,c(all.vars(formula)[1])]) 
  prioris <- generate_priori_simplex(levels_present, step = 0.1)
  for(n_labled in label) { 
    for(n_unlabled in unlabel) {
      for(alpha in alp) {
        
        
        print(paste("RUNNING on ",workers, " workers: data ", dat, " n_labled: ", n_labled, " n_unlabled: ", n_unlabled, " alpha: ", alpha, sep= ""))
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
            
            SSL <- refernce_SSL(train, unlabeld, test, priori = NULL)
            SL <- refernce_SL(train, unlabeld, test, priori = NULL) 
            
            l <- list(e_admissible=e_admissible,SSL = SSL, SL= SL )
            
            # Fortschritt updaten
            p(sprintf("Zeile %d bearbeitet", i))
            
            return(l)
          })
        })
        gamma <- unlist(gamma_l, recursive = FALSE)
        
        
        path = paste(getwd(),"/NaiveBayes/results_NB/", dat,"_L_", as.character(n_labled), "_U_",  as.character(n_unlabled), "_alp_" , as.character(alpha), sep="")
        save(gamma, file = path)

        time_b <- Sys.time()
        time_diff <- as.character(time_b-time_a)
        print(paste("DONE ", time_diff))
        
        
        
      }
    }
    
  }
}

