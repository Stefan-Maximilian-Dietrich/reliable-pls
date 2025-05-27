
# 1. Generator erstellen 
tg = tgen("cassini")

# 2. Dann Datensatz mit n Punkten erzeugen
set.seed(1)
task = tg$generate(1000)

# 3. Daten extrahieren
data = task$data()

names(data)[1] <- "target"
target <- data$target

data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:3) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()