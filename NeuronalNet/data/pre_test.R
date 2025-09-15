library(mlr3verse)
dt <- as.data.table(mlr_tasks)
dt_class <- dt[dt$task_type == "classif",]
View(dt_class)


#### möglich
task = tsk("spam")
data = task$data()
names(data)[1] <- "target"
target <- data$target

data <- as.data.frame(data)

A <- sapply(data, function(x) length(unique(x))) 
data <- data[, A>400]
vars <- c("target ~")
for (v in 1:7) {
  vars <- c(vars, colnames(data)[v])
}
data <- cbind(target, data) 
formula = paste(vars, collapse=" + ") %>% as.formula()

### sehr gut
task = tsk("wine")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
nrow(data)
data <- as.data.frame(data)

vars <- c("target ~")
for (v in 2:13) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

### nein!
task = tsk("sonar")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
nrow(data)
data <- as.data.frame(data)

vars <- c("target ~")
for (v in 2:58) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

## gut ## adddd DONE
task = tsk("pima")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
A
nrow(data)
data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:8) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()


### yes yes yes ### include
task = tsk("penguins_simple")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
A
nrow(data)
data <- as.data.frame(data)
data <- na.omit(data)[1:5]

vars <- c("target ~")
for (v in 2:5) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

### nein!
task = tsk("optdigits")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
A
nrow(data)
data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:65) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()


#### nein!
task = tsk("ilpd")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
A
nrow(data)
data <- as.data.frame(data)
data <- na.omit(data)[,-c(9)]

vars <- c("target ~")
for (v in 2:10) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()



####### sehr gut 
library(mlr3)
library(mlr3data)

library(mlr3)

# 1. Generator erstellen
tg = tgen("2dnormals")

# 2. Dann Datensatz mit n Punkten erzeugen
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
ggplot(data, aes(x = x1, y = x2, color = target)) +
  geom_point() +
  labs(title = "Moons Datensatz") +
  theme_minimal()

################ gut !
library(mlr3)
library(mlr3data)

library(mlr3)

# 1. Generator erstellen
tg = tgen("moons")

# 2. Dann Datensatz mit n Punkten erzeugen
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
ggplot(data, aes(x = x1, y = x2, color = target)) +
  geom_point() +
  labs(title = "Moons Datensatz") +
  theme_minimal()

###### extem gut 
library(mlr3)
library(mlr3data)

library(mlr3)

# 1. Generator erstellen 
tg = tgen("cassini")

# 2. Dann Datensatz mit n Punkten erzeugen
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
ggplot(data, aes(x = x1, y = x2, color = target)) +
  geom_point() +
  labs(title = " Datensatz") +
  theme_minimal()
###
###### extem extrem gut 
library(mlr3)
library(mlr3data)

library(mlr3)
d <- 10
# 1. Generator erstellen 
tg = tgen("simplex", d = d)

# 2. Dann Datensatz mit n Punkten erzeugen
task = tg$generate(20000)

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

##########################

library(mlr3)
library(mlr3data)

library(mlr3)
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

########################## geht gar nicth 
d <- 4
# 1. Generator erstellen 
tg = tgen("xor", d = d)

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

##########################
# URL zum Datensatz
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/dermatology/dermatology.data"

# Datensatz laden (NA-Werte sind durch "?" kodiert)
dermatology <- read.csv(url, header = FALSE, na.strings = "?")

# Spaltennamen laut UCI (34 Merkmale + 1 Klasse)
colnames(dermatology) <- c(
  "erythema", "scaling", "definite_borders", "itching", "koebner_phenomenon",
  "polygonal_papules", "follicular_papules", "oral_mucosal_involvement",
  "knee_and_elbow_involvement", "scalp_involvement", "family_history",
  "melanin_incontinence", "eosinophils_in_the_infiltrate", "PNL_infiltrate",
  "fibrosis_of_the_papillary_dermis", "exocytosis", "acanthosis",
  "hyperkeratosis", "parakeratosis", "clubbing_of_the_rete_ridges",
  "elongation_of_the_rete_ridges", "thinning_of_the_suprapapillary_epidermis",
  "spongiform_pustule", "munro_microabcess", "focal_hypergranulosis",
  "disappearance_of_the_granular_layer", "vacuolisation_and_damage_of_basal_layer",
  "spongiosis", "saw_tooth_appearance_of_retes", "follicular_horn_plug",
  "perifollicular_parakeratosis", "inflammatory_monoluclear_inflitrate",
  "band_like_infiltrate", "age", "class"
)

# Vorschau anzeigen
head(dermatology)

# Struktur überprüfen
str(dermatology)

# Zielklassen anzeigen
table(dermatology$class)