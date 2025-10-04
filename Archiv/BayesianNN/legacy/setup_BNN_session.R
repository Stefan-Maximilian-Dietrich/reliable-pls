# Liste der Pakete zum Installieren und Laden
pakete <- c(
  "naivebayes",   # Paket für Naive Bayes Klassifikation
  "caret",        # Paket für maschinelles Lernen und Modelltraining
  "tidyverse",    # Sammlung von Paketen für Datenmanipulation und -visualisierung
  "doParallel",   # Paket für paralleles Rechnen
  "dplyr",        # Paket für Datenmanipulation und Filterung
  "furrr",        # Paket für parallele Ausführung von purrr-Operationen
  "progressr",    # Paket zur Verfolgung des Fortschritts von langen Berechnungen
  "tibble",       # Paket für das Arbeiten mit data.frame-ähnlichen Objekten
  "partitions",   # Paket für die Arbeit mit Partitionen und Kombinationen
  "MixGHD",       # Paket für MixGHD-Methoden
  "RCurl",        # Paket für das Abrufen von Daten aus dem Web
  "mlbench",      # Paket für maschinelles Lernen mit Benchmark-Datensätzen
  "fdm2id",       # Paket für den Zugriff auf die fdm2id-Daten
  "profvis",
  "mlr3verse"
  # Paket zur Profilerstellung und Leistungsanalyse von R-Code
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

source(paste(getwd(),"/NaiveBayes/Function_NB.R", sep = ""))
source(paste(getwd(),"/NaiveBayes/Methods.R", sep = ""))

cat("Session ready")
