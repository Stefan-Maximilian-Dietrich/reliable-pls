# URL des Digits-Datensatzes (UCI Repository)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/optdigits/optdigits.tra"

# Lade die Daten
digits <- read.table(url, sep=",", header=FALSE)

# Spaltennamen setzen
colnames(digits) <- c(paste0("Pixel", 1:64), "Label")

# Zeige die ersten Zeilen
head(digits)