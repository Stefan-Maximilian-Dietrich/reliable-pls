#########
# Vollständiges Beispiel: parallele Verarbeitung mit Fortschrittsbalken

# Pakete laden (vorher ggf. installieren mit install.packages())
library(furrr)
library(progressr)
library(tibble)
library(dplyr)

# 1. Beispiel-Datensatz erzeugen
N = 8 

# 2. Parallelisierungsstrategie festlegen (plattformunabhängig)
plan(multisession, workers = 4)

# 3. Fortschrittsbalken aktivieren
handlers("txtprogressbar")  # andere möglich: "cli", "rstudio", "progress"

# 4. Verarbeitung mit Fortschrittsanzeige
with_progress({
  p <- progressor(steps = nrow(df))  # Fortschritt explizit setzen
  
  result <- future_map(1:N, function(i) {
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

flat_list <- unlist(result, recursive = FALSE)

# 5. Ergebnis anzeigen
result <- unlist(result)
names(result) <- paste0("Zeile_", 1:length(result))
print(result)


a <- Sys.time()
b <- Sys.time()
print(b-a)



time_a <- Sys.time()

# --- dein Prozess hier ---
Sys.sleep(72.53)  # Beispiel: simuliert eine Rechenzeit
# ------------------------

time_b <- Sys.time()

# Zeitdifferenz in Sekunden berechnen
duration <- as.numeric(difftime(time_b, time_a))

# Ausgabe formatieren
cat(sprintf("DONE in %.2f seconds\n", duration))