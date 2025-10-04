# URL des Digits-Datensatzes (UCI Repository)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tra"

# Lade die Daten
digits <- read.table(url, sep=",", header=FALSE)

# Spaltennamen setzen
colnames(digits) <- c(paste0("Pixel", 1:64), "Label")

# Zeige die ersten Zeilen
head(digits)

data <- digits[c(65, 1:64)]

names(data)[names(data) == 'Label'] <- 'target'

vars <- names(data)

# erste Spalte = Zielvariable
response <- vars[1]

# restliche Spalten = Prädiktoren
predictors <- vars[-1]

# Formel zusammensetzen
formula <- as.formula(
  paste(response, "~", paste(predictors, collapse = " + "))
)