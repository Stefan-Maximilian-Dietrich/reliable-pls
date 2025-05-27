source(paste(getwd(),"/NaiveBayes/MASTER/standart_experimente.R", sep = ""))
for(i in 1:length(Experiments)) {
  titel <- paste0(getwd(), "/NaiveBayes/MASTER/results/" ,Experiments[[i]]$data, "_L_", Experiments[[i]]$L, "_U_",  Experiments[[i]]$U, "_alp_", Experiments[[i]]$alp, "_", Experiments[[i]]$prio_t, "_", Experiments[[i]]$prio_r)
  dir.create(titel)
}

folder_path <-  paste(getwd(),"/NaiveBayes/MASTER/results/", sep="")
files <- list.files(folder_path, full.names = TRUE)

# Namen der Unterordner
unterordner <- c("e_admissible", "SSL", "SL",  "SSL_variance", "SSL_entropy", "maximal", "M_MaxiMin", "M_MaxiMax")

# Für jeden Ordner die Unterordner anlegen
for (ordner in files) {
  for (u in unterordner) {
    pfad <- file.path(ordner, u)
    dir.create(pfad, recursive = TRUE, showWarnings = FALSE)
  }
}


check_done <-  function() {
  results_adress <- paste0(getwd(),"/NaiveBayes/MASTER/results")
  df <- do.call(rbind, lapply(Experiments, as.data.frame))
  
  check_df <- data.frame()

  for(i in 1:nrow(df)) {
    flags <- setNames(rep(FALSE, length(names(methods))), names(methods))
    for(meth in names(methods)) {
      adr <- paste0(results_adress,"/", experiment_to_adress(df[i,]),"/", meth )
      number <- length(list.files(adr, full.names = TRUE))
      if(number >= 60) {
        flags[meth] <- TRUE
      } else {
        flags[meth] <- FALSE
      }
      
    }
    check_df <- rbind(check_df, flags)
  }
  names(check_df) <- names(methods)
  inProgress <- FALSE
  overall <- apply(check_df, 1, all)
  result <- cbind(df, inProgress,overall, check_df)
  
}

path <-  paste(getwd(),"/NaiveBayes/MASTER/CHECK", sep="")
CHECK <- result
save(CHECK, file = path)

path <-  paste(getwd(),"/NaiveBayes/MASTER/SEMAPHOR", sep="")
SEMAPHOR <- TRUE
save(SEMAPHOR, file = path)

