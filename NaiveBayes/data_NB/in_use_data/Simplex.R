d <- 10
# 1. Generator erstellen 
tg = tgen("simplex", d = d)

# 2. Dann Datensatz mit n Punkten erzeugen
task = tg$generate(200000)

# 3. Daten extrahieren
data = task$data()

names(data)[1] <- "target"
target <- data$target

data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:(d+1)) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()