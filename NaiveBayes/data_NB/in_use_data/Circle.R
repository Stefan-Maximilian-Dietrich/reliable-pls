d <- 20
# 1. Generator erstellen 
tg = tgen("circle", d = d)

# 2. Dann Datensatz mit n Punkten erzeugen
task = tg$generate(2000)

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
ggplot(data, aes(x = x1, y = x2, color = target)) +
  geom_point() +
  labs(title = " Datensatz") +
  theme_minimal()