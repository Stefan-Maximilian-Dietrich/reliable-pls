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

for(path in files) {
  load(path)
  mathods <- unique(names(gamma))
  if(any(mathods == "visible")) {
    delete <- seq(from = 2, to = length(gamma), by = 2)
    gamma[delete] <- NULL
    names(gamma) <- rep("e_admissible", length(gamma)) ##### liste von hinten gepaddet werden 
  }
  
}
