# Beispiel: Liste mit 'value' Vektoren und 'visible' Werten
ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
for(path in files) {
  print(path)
  load(path)
  mathods <- unique(names(gamma))

  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte <- lapply(matrizen, colMeans)
  
  # Umwandlung in ein Dataframe für ggplot
  df <- do.call(rbind, lapply(names(spalten_mittelwerte), function(name) {
    data.frame(Name = name, X = 1:length(spalten_mittelwerte[[name]]), 
               Mittelwert = spalten_mittelwerte[[name]])
  }))
  df$X <- df$X-1
  
  bereinigter_string <- sub(".*/", "", path)
  
  # Plot erstellen
  plot_object <- ggplot(df, aes(x = X, y = Mittelwert, color = Name, group = Name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "unlabed Data", y = "Test accuracy", title = bereinigter_string)  

    
 
  ggsave(paste("/Users/Stefan/Desktop/Studium/Publikation/Experimetn_Grafiken/", bereinigter_string, ".png", sep = ""), plot = plot_object)
  
}

# Beispiel: Liste mit 'value' Vektoren und 'visible' Werten
ground_path <-  paste(getwd(),"/NaiveBayes/results_NB", sep="")
files <- list.files(path = ground_path, full.names = TRUE, recursive = TRUE)
improvment_matrix <- NULL
for(path in files) { # anteil der fehler im vergleich zu SL 0 = alle fehler beseitigt 1= genua so viele fehler 2= doppelt so vile fehler 
  print(path)
  load(path)
  mathods <- unique(names(gamma))
  
  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte <- lapply(matrizen, colMeans)
  
  ead_end <- spalten_mittelwerte$e_admissible[length(spalten_mittelwerte$e_admissible)]
  erow_ref_point <- 1- spalten_mittelwerte$SL[1]
  errow_quote <- lapply(spalten_mittelwerte, function(x) 1- x[length(x)])
  improvement <- lapply(errow_quote, function(x)  x/erow_ref_point)
  ratio <- unlist(improvement)
  
  numbers <- as.numeric(unlist(regmatches(path, gregexpr("[0-9]+(?:\\.[0-9]+)?", path))))
  names(numbers) <- c("L", "U", "alp")
  bereinigter_string <- sub(".*/", "", path)
  vec <- c(numbers, e_ad_end = ead_end, ratio)
  prefix <- sub("_.*", "", bereinigter_string)
  df <- data.frame(data = prefix, as.list(vec))
  improvment_matrix <- rbind(improvment_matrix, df)
  
  
}

View(improvment_matrix)

