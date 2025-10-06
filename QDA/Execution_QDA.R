while(TRUE){
  #geschützer Bereich
  experiemnt <- get_experiment(methods) 
  change_progress(experiemnt, bool = TRUE)
  #
  
  
  dat        <- experiemnt$data
  n_labled   <- experiemnt$L
  n_unlabled <- experiemnt$U
  alpha      <- experiemnt$alp
  priori_tec <- experiemnt$prio_t
  refinement <- experiemnt$prio_r
  
  data_loader(dat)
  
  # Plan & Map-Funktion abhängig vom Schalter setzen
  if (use_parallel) {
    plan(multisession, workers = workers)
    map_fn <- furrr::future_map
    nworkers_print <- workers
  } else {
    plan(sequential)
    map_fn <- purrr::map
    nworkers_print <- 1
  }
  
  modus <- if (use_parallel) "parallel" else "sequentiell"
  message(sprintf(
    "RUNNING (%s) on %d workers: data %s n_labled: %s n_unlabled: %s alpha: %s prioris: %s",
    modus, nworkers_print, dat, n_labled, n_unlabled, alpha, refinement
  ))
  
  time_a <- Sys.time()
  
  # Fortschrittsbalken
  handlers("txtprogressbar")  # Alternativen: "cli", "rstudio", "progress"
  
  with_progress({
    p <- progressor(steps = N)
    
    map_fn(1:N, function(i){
      tryCatch({
        set.seed(i)
        
        sample <- sampler_NB_up(n_labled, n_unlabled, data, formula)
        data_train     <- sample[[1]]
        data_unlabeled <- sample[[2]]
        data_test      <- sample[[3]]
        
        priors <- generate_random_priors(
          data = data_train,
          n_priors    = refinement,
          kappa_range = c(0.1, 3),
          scale_range = c(0.5, 3),
          nu_extra    = c(0, 5, 10)
        )
        
        # --- Methoden ausführen & speichern --------------------------------
        if (methods$e_admissible){
          e_admissible <- e_admissible_SSL(priors, data_train, data_unlabeled, data_test, alpha)
          save(e_admissible, file = .mk_title(save_path, experiemnt, "e_admissible", i))
          method_done(experiemnt, method = "e_admissible") 
          
        }
        
        if (methods$maximal){
          maximal <- maximal_SSL(priors, data_train, data_unlabeled, data_test, alpha)
          save(maximal, file = .mk_title(save_path, experiemnt, "maximal", i))
          method_done(experiemnt, method = "maximal") 
          
        }
        
        if (methods$SSL){
          SSL <- refernce_SSL(data_train, data_unlabeled, data_test)
          save(SSL, file = .mk_title(save_path, experiemnt, "SSL", i))
          method_done(experiemnt, method = "SSL") 
          
        }
        
        if (methods$SL){
          SL <- refernce_SL(data_train, data_unlabeled, data_test)
          save(SL, file = .mk_title(save_path, experiemnt, "SL", i))
          method_done(experiemnt, method = "SL") 
          
        }
        
        if (methods$SSL_variance){
          SSL_variance <- refernce_SSL_variance(data_train, data_unlabeled, data_test)
          save(SSL_variance, file = .mk_title(save_path, experiemnt, "SSL_variance", i))
          method_done(experiemnt, method = "SSL_variance") 
          
        }
        
        if (methods$SSL_entropy){
          SSL_entropy <- refernce_SSL_entropy(data_train, data_unlabeled, data_test)
          save(SSL_entropy, file = .mk_title(save_path, experiemnt, "SSL_entropy", i))
          method_done(experiemnt, method = "SSL_entropy") 
          
        }
        
        if (methods$M_MaxiMin){
          M_MaxiMin <- M_MaxiMin_SSL(priors, data_train, data_unlabeled, data_test, alpha)
          save(M_MaxiMin, file = .mk_title(save_path, experiemnt, "M_MaxiMin", i))
          method_done(experiemnt, method = "M_MaxiMin") 
          
        }
        
        if (methods$M_MaxiMax){
          M_MaxiMax <- M_MaxiMax_SSL(priors, data_train, data_unlabeled, data_test, alpha)
          save(M_MaxiMax, file = .mk_title(save_path, experiemnt, "M_MaxiMax", i))
          method_done(experiemnt, method = "M_MaxiMax") 
          
        }
        # --------------------------------------------------------------------
        
      }, error = function(e){
        msg <- sprintf("Fehler bei Seed/Index %d: %s", i, e$message)
        message(msg)
        if (!is.null(error_log_file)) {
          try(write(paste0(format(Sys.time()), " ", msg, "\n"),
                    file = error_log_file, append = TRUE), silent = TRUE)
        }
      }, finally = {
        # Fortschritt immer updaten, auch bei Fehler
        p(sprintf("Zeile %d bearbeitet", i))
      })
      
      invisible(NULL)
    })
  })
  
  #### Plot
  experiment_path <- paste0(
    "/dss/dsshome1/03/di35lox/MASTER/results/QDA/",
    experiemnt$data, "_L_", experiemnt$L, "_U_", experiemnt$U,
    "_alp_", experiemnt$alp, "_", experiemnt$prio_t, "_", experiemnt$prio_r
  )
  make_Result_Graph(experiment_path)
  
  #geschützer Bereich 
  change_progress(experiemnt, bool = FALSE) 
  ####
  
  time_b <- Sys.time()
  duration_function(time_a, time_b)
  
  # Cluster nur stoppen, wenn parallel gelaufen
  if (use_parallel) try(future:::ClusterRegistry("stop"), silent = TRUE)
}
