# Liste der Pakete zum Installieren und Laden
pakete <- c(
  "MASS",
  "matrixStats",    # Paket zur Verfolgung des Fortschritts von langen Berechnungen
  "dplyr",        # Paket für parallele Ausführung von purrr-Operationen
  "mvtnorm",
  "caret",
  "MixGHD", 
  "progressr",    # Paket zur Verfolgung des Fortschritts von langen Berechnungen
  "furrr",
  "mclust"
)

# Schleife, um Pakete zu installieren und zu laden
for (paket in pakete) {
  # Überprüfen, ob das Paket installiert ist, und es nur installieren, wenn nicht
  if (!requireNamespace(paket, quietly = TRUE)) {
    install.packages(paket)
  }
  # Paket laden
  library(paket, character.only = TRUE)
}

source(paste(getwd(),"/QDA/Functions_QDA.R", sep = ""))
source(paste(getwd(),"/QDA/Methods_QDA.R", sep = ""))

cat("Session ready")
