data_discrete
library(data.table) # check if mulit thered is possible online 

dt_data_discrete <- as.data.table(data_discrete)

freq_prior <- function(data_discrete) {
  freq <- dt_data_discrete[, .N, by= target][, N / sum(N)]
}

P_X_C <- function(data_discrete) {

  long_dt <-  data.table::melt(dt_data_discrete, id.vars = "target", variable.name = "feature_name",  value.name = "feature_value")
  result <- long_dt[, .N, by = .(feature_A_value = target, feature_name, feature_value)]
  result[, rel_freq := N / sum(N), by = .(feature_A_value, feature_name)]
  setorder(result, feature_A_value)
  
  # Gruppieren: Zählen wie oft jede Kombination auftritt
  
}


P_X_YC <- function(data_discrete) {
  feature_name <- "bill_length"
  
  # Bringe nur Feature B in Long-Format
  long_dt <- dt_data_discrete[, .(target, body_mass, feature_value = get(feature_name))]
  
  # Zählen der Kombinationen (A, Z, Feature B)
  result <- long_dt[, .N, by = .(target, body_mass, feature_value)]
  
  # Relative Häufigkeit innerhalb (A, Z)
  result[, rel_freq := N / sum(N), by = .(target, body_mass)]
  
  # Sortieren nach A und Z
  setorder(result, target, body_mass)
}
######



# Beispiel-Datentabelle mit zwei "bedingenden" Spalten A und Z
dt <- data.table(
  A = c("a", "a", "b", "b", "a", "b", "a"),
  Z = c(1, 1, 1, 2, 2, 2, 1),
  B = c("x", "y", "x", "x", "y", "y", "x"),
  C = c("low", "high", "low", "high", "low", "low", "high")
)

# Wähle das spezifische Feature B
feature_name <- "B"

# Bringe nur Feature B in Long-Format
long_dt <- dt[, .(A, Z, feature_value = get(feature_name))]

# Zählen der Kombinationen (A, Z, Feature B)
result <- long_dt[, .N, by = .(A, Z, feature_value)]

# Relative Häufigkeit innerhalb (A, Z)
result[, rel_freq := N / sum(N), by = .(A, Z)]

# Sortieren nach A und Z
setorder(result, A, Z)