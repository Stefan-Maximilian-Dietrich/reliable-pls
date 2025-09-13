# --- Pakete installieren (falls noch nicht installiert) ---
# greta benötigt TensorFlow im Hintergrund
install.packages("greta")
install.packages("gretaExtras")  # Hilfsfunktionen
# Falls Probleme mit greta: https://greta-stats.org

# --- Pakete laden ---
library(greta)
library(gretaExtras)

# --- Daten vorbereiten ---
data(iris)
# Inputs
x <- as.matrix(iris[, 1:4])
# One-hot Encoding der Zielvariable
y <- to_categorical(as.integer(iris$Species) - 1)

# --- Netzwerk-Architektur festlegen ---
n_input  <- ncol(x)       # 4
n_hidden <- 5             # Anzahl Hidden Units (kann variiert werden)
n_output <- ncol(y)       # 3 Klassen

# --- Priors für Gewichte und Biases ---
W1 <- greta::normal(0, 1, dim = c(n_input,  n_hidden))
b1 <- greta::normal(0, 1, dim = c(1,         n_hidden))
W2 <- greta::normal(0, 1, dim = c(n_hidden,   n_output))
b2 <- greta::normal(0, 1, dim = c(1,         n_output))

# --- Vorwärtsdurchlauf ---
hidden_lin  <- x %*% W1 + b1        # lineare Hidden Units
hidden_act  <- greta::tanh(hidden_lin)  # Aktivierung (tanh)

logits      <- hidden_act %*% W2 + b2   # Ausgabeschicht (logits)
p           <- greta::softmax(logits)   # Klassenwahrscheinlichkeiten

# --- Likelihood ---
distribution(y) <- categorical(p)

# --- Modell zusammenstellen ---
model <- greta::model(W1, b1, W2, b2)

# --- MCMC konfigurieren & starten ---
draws <- greta::mcmc(
  model,
  n_samples   = 2000,
  warmup      = 1000,
  chains      = 4,
  verbose     = TRUE
)

# --- Posterior-Vorhersagen ---
post_probs <- calculate(p, draws = draws)

# Für jede Beobachtung Mittelwert der Posterior-p-Werte
mean_probs <- apply(post_probs, c(1,2), mean)

# Predicted class = Index des Maximums
pred_class <- max.col(mean_probs)  # 1,2,3
true_class <- as.integer(iris$Species)

# --- Performance berechnen ---
conf_matrix <- table(Predicted = pred_class, Actual = true_class)
print(conf_matrix)

accuracy <- sum(pred_class == true_class) / nrow(iris)
print(sprintf("Accuracy: %.3f", accuracy))
