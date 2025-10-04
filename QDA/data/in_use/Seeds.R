url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt"

# Datensatz laden
seeds <- read.table(url, header = FALSE)

# Spaltennamen setzen (laut UCI Beschreibung)
colnames(seeds) <- c(
  "area",
  "perimeter",
  "compactness",
  "kernel_length",
  "kernel_width",
  "asymmetry_coefficient",
  "kernel_groove_length",
  "class"
)

# Vorschau anzeigen 

data <- as.data.frame(seeds)
data <- na.omit(data)[, c(8, 1:7)]
names(data)[1] <- "target"
data$target <- as.factor(data$target)
vars <- c("target ~")
for (v in 2:8) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()
data <- data[, all.vars(formula)]
