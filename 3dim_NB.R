library(e1071)

# Lade den iris-Datensatz
data(iris)

# Teile die Daten in Trainings- und Testsets (80%-20% Aufteilung)
set.seed(123)
train_idx <- sample(1:nrow(iris), size = 0.8 * nrow(iris))
train_data <- iris[train_idx, ]
test_data <- iris[-train_idx, ]

# Definiere eigene Prior-Wahrscheinlichkeiten
custom_priors <- c(setosa = 0.5, versicolor = 0.3, virginica = 0.2)

# Trainiere das Naive-Bayes-Modell
model <- naiveBayes(formula = Species ~ ., data = train_data, prior = custom_priors)

# Mache Vorhersagen mit Posterior-Wahrscheinlichkeiten
posterior_probs <- predict(model, test_data, type = "raw")
likelihood <- prod(apply(posterior_probs, 1, max))

# Zeige die ersten paar Posterior-Wahrscheinlichkeiten
print(head(posterior_probs))

# Kombiniere die Wahrscheinlichkeiten mit den tatsächlichen Labels und der Vorhersage
results <- data.frame(
  Actual = test_data$Species,
  Predicted = predict(model, test_data),
  Posterior_Prob_Setosa = posterior_probs[, "setosa"],
  Posterior_Prob_Versicolor = posterior_probs[, "versicolor"],
  Posterior_Prob_Virginica = posterior_probs[, "virginica"]
)

# Zeige die ersten Zeilen des Ergebnisses
print(head(results))
