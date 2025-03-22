
path = paste(getwd(),"/NB_eadmissible/", "experiment_","n_labled_", as.character(4), "_n_unlabled_",  as.character(2), "_alpha_" , as.character(0.1), sep="")


for(n_labled in c(4,6,8,10,12)) { 
  for(n_unlabled in c(2,4,8,16,32,64,128)) {
    for(alpha in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)) {
      
    }
  }
  
}

# Beispiel: Liste mit 'value' Vektoren und 'visible' Werten
data_list <- list(
  list(value = c(0.984, 0.984, 0.984, 0.989, 0.989), visible = TRUE),
  list(value = c(0.952, 0.952, 0.952, 0.952, 0.952), visible = TRUE),
  list(value = c(0.989, 0.989, 0.989, 0.989, 0.989), visible = TRUE)
)

# Extrahiere die 'value'-Elemente
value_matrix <- do.call(list, lapply(gamma, function(x) x$value))

load(path)
str(gamma) 