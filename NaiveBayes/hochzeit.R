

mother <- paste(getwd(),"/NaiveBayes/results_NB_PC/Penguins_L_12_U_256_alp_0.4_prio_grid_20", sep="")
son <- "/Users/Stefan/Downloads/Penguins_L_12_U_256_alp_0.4_prio_grid_20_adMM"

load(mother)
gamma_m <- gamma
load(son) 
gamma_s <- gamma
done_methods <- unique(names(gamma_m[!is.na(gamma_m)]))
new_methods <- unique(names(gamma_s))
to_transfer <- setdiff(new_methods, done_methods)
transfer_methods <- gamma_s[new_methods %in% to_transfer]
behalten_s <- unlist(sapply(transfer_methods, function(x) !is.logical(x)))
behalten_m <- unlist(sapply(gamma_m, function(x) !is.logical(x)))

gamma <- c(gamma_m[behalten_m], transfer_methods[behalten_s])


mathods <- unique(names(gamma))
gruppen <- split(gamma, unique(names(gamma)))


matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
spalten_mittelwerte <- lapply(matrizen, colMeans)

# Umwandlung in ein Dataframe für ggplot
df <- do.call(rbind, lapply(names(spalten_mittelwerte), function(name) {
  data.frame(Name = name, X = 1:length(spalten_mittelwerte[[name]]), 
             Mittelwert = spalten_mittelwerte[[name]])
}))
df$X <- df$X-1

bereinigter_string <- sub(".*/", "", mother)


titel_d <- titel_data(mother)
titel_l <- extrahiere_zahl_L(mother)
titel_u <- extrahiere_zahl_U(mother)
plot_object <- ggplot(df, aes(x = X, y = Mittelwert, color = Name, group = Name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "unlabed Data", y = "test accuracy", title = paste0(titel_d, ": labeled = ", titel_l, ", unlabeled = ", titel_u))  

print(plot_object)


################################################################################
ground_path_results <-  paste(getwd(),"/NaiveBayes/results_NB_PC", sep="")
files_result <- list.files(path = ground_path_results, full.names = TRUE, recursive = TRUE)
ground_path_new <-  paste(getwd(),"/NaiveBayes/new", sep="")
files_new <- list.files(path = ground_path_new, full.names = TRUE, recursive = TRUE)

bese <- files_new[grepl("variance$", files_new)]

path <- bese[10]
for(path in bese){
  load(path)
  gamma_base <- gamma
  
  filtered_gamma_baes <- Filter(function(x) !is.na(x[1]), gamma_base)
  mathods <- unique(names(filtered_gamma_baes))
  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte_new_base <- lapply(matrizen, colMeans)
  
  name <-  sub(".*/", "", path)
  name1 <- sub("_ad.*", "", name)


  gamma_alt <- tryCatch(
    {
      new_path <-  paste(getwd(),"/NaiveBayes/results_NB_PC/",name1, sep="")
      load(alt_path)
      gamma  # explizit zurückgeben
    },
    error = function(e) {
      # Alternativwert zurückgeben
      NA
    }
  )
  
  filtered_gamma_alt <- Filter(function(x) !is.na(x[1]), gamma_alt)
  mathods <- unique(names(filtered_gamma_alt))
  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte_new_alt <- lapply(matrizen, colMeans)
  
  gamma_new <- tryCatch(
    {
      new_path <- paste(getwd(), "/NaiveBayes/new/", name1, "_adMM", sep = "")
      load(new_path)
      gamma  # explizit zurückgeben
    },
    error = function(e) {
      # Alternativwert zurückgeben
      NA
    }
  )
  
  filtered_gamma_new <- Filter(function(x) !is.na(x[1]), gamma_new)
  mathods <- unique(names(filtered_gamma_new))
  gruppen <- split(gamma, mathods)
  matrizen <- lapply(gruppen, function(x) do.call(rbind, x))
  spalten_mittelwerte_new <- lapply(matrizen, colMeans)
  
  
  if(is.null( spalten_mittelwerte_new_alt$SL[1] )) {
    alt_bool <- FALSE
  } else {
    alt_bool <- spalten_mittelwerte_new_base$SL[1] ==  spalten_mittelwerte_new_alt$SL[1] 
    
  }
  
  if(is.null(spalten_mittelwerte_new$SL[1] )) {
    neu_bool <- FALSE
  } else {
    neu_bool <- spalten_mittelwerte_new_base$SL[1] ==  spalten_mittelwerte_new$SL[1] 
    
  }
  
  
  
  SL <- filtered_gamma_baes[names(filtered_gamma_baes) == "SL"]
  SSL_entropy <- filtered_gamma_baes[names(filtered_gamma_baes) == "SSL_entropy"]
  SSL_variance <- filtered_gamma_baes[names(filtered_gamma_baes) == "SSL_variance"]
  
  if(alt_bool) {
    e_admissible <- filtered_gamma_alt[names(filtered_gamma_alt) == "e_admissible"]
    maximal <- filtered_gamma_alt[names(filtered_gamma_alt) == "maximal"]
  } else {
    e_admissible <- NA
    maximal <- NA
  }
  
  if(neu_bool) {
    M_MaxiMax <- filtered_gamma_new[names(filtered_gamma_new) == "M_MaxiMax"]
    M_MaxiMin <- filtered_gamma_new[names(filtered_gamma_new) == "M_MaxiMin"]
  } else {
    M_MaxiMax <- NA
    M_MaxiMin <- NA
  }
  
  gamma <- c(SL, SSL_entropy, SSL_variance, e_admissible, maximal, M_MaxiMax, M_MaxiMin)
  save(gamma, file =  paste(getwd(),"/NaiveBayes/results_NB_PC1/",name1, sep=""))
  
}


################################################################################
gp <-  paste(getwd(),"/NaiveBayes/results_NB_PC1", sep="")
files_result <- list.files(path = gp, full.names = TRUE, recursive = TRUE)

path <- files_result[1]
for(path in files_result) {
  load(path) 
  
  if(length(gamma[names(gamma) == "SL"])  > 0) {
    SL <- (gamma[names(gamma) == "SL"]) 
  } else {
    SL <- list(SL = NA )
  }

  if(length(gamma[names(gamma) == "SSL_entropy"])  > 0) {
    SSL_entropy <- (gamma[names(gamma) == "SSL_entropy"]) 
  } else {
    SSL_entropy <- list(SSL_entropy = NA )
  }
  
  if(length(gamma[names(gamma) == "SSL_variance"])  > 0) {
    SSL_variance <- (gamma[names(gamma) == "SSL_variance"]) 
  } else {
    SSL_variance <- list(SSL_variance = NA )
  }
  
  if(length(gamma[names(gamma) == "e_admissible"])  > 0) {
    e_admissible <- (gamma[names(gamma) == "e_admissible"]) 
  } else {
    e_admissible <- list(e_admissible = NA )
  }
  
  if(length(gamma[names(gamma) == "maximal"])  > 0) {
    maximal <- (gamma[names(gamma) == "maximal"]) 
  } else {
    maximal <- list(maximal = NA )
  }
  
  if(length(gamma[names(gamma) == "M_MaxiMax"])  > 0) {
    M_MaxiMax <- (gamma[names(gamma) == "M_MaxiMax"]) 
  } else {
    M_MaxiMax <- list(M_MaxiMax = NA )
  }
  
  if(length(gamma[names(gamma) == "M_MaxiMin"])  > 0) {
    M_MaxiMin <- (gamma[names(gamma) == "M_MaxiMin"]) 
  } else {
    M_MaxiMin <- list(M_MaxiMin = NA )
  }
  
  gamma <- c(SL, SSL_entropy, SSL_variance, e_admissible, maximal, M_MaxiMax, M_MaxiMin)
  save(gamma, file = path)
  

}
