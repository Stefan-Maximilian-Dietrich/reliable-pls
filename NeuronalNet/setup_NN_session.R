# Liste der Pakete zum Installieren und Laden
pakete <- c(
  "numDeriv",
  "progressr",    # Paket zur Verfolgung des Fortschritts von langen Berechnungen
  "furrr",        # Paket für parallele Ausführung von purrr-Operationen
  "caret",
  "MixGHD",
  "tidyverse", 
  "mlr3",
  "RCurl",
  "MASS"
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

source(paste(getwd(),"/NeuronalNet/FunctionsNN.R", sep = ""))
source(paste(getwd(),"/NeuronalNet/Methods_NN.R", sep = ""))

cat("Session ready")


