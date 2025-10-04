# 📥 1. Iris-Daten laden
data(iris)

# 🔀 2. Train-Test-Split (z. B. 70/30)
n <- nrow(iris)
train_idx <- sample(1:n, size = 0.7 * n)
train_data <- iris[train_idx, ]
test_data <- iris[-train_idx, ]

# 📌 3. Funktionen zum Trainieren und Anwenden von GNB

# Funktion: Gaussian PDF
gauss_pdf <- function(x, mu, sigma2) {
  return((1 / sqrt(2 * pi * sigma2)) * exp(- (x - mu)^2 / (2 * sigma2)))
}

# Funktion: Trainiere GNB
train_gnb <- function(data, label_col) {
  labels <- data[[label_col]]
  features <- data[, setdiff(names(data), label_col)]
  classes <- unique(labels)
  
  model <- list()
  model$classes <- classes
  model$priors <- table(labels) / length(labels)
  model$params <- list()
  
  for (cls in classes) {
    subset <- features[labels == cls, ]
    mu <- colMeans(subset)
    sigma2 <- apply(subset, 2, var)
    model$params[[as.character(cls)]] <- list(mu = mu, sigma2 = sigma2)
  }
  
  return(model)
}

# Funktion: Vorhersage
predict_gnb <- function(model, newdata) {
  predictions <- c()
  
  for (i in 1:nrow(newdata)) {
    x <- newdata[i, ]
    log_probs <- c()
    
    for (cls in model$classes) {
      params <- model$params[[as.character(cls)]]
      mu <- params$mu
      sigma2 <- params$sigma2
      
      # log(P(y)) + sum log(P(x_j | y))
      log_prior <- log(model$priors[[as.character(cls)]])
      log_likelihood <- sum(log(gauss_pdf(as.numeric(x), mu, sigma2)))
      
      log_probs <- c(log_probs, log_prior + log_likelihood)
    }
    
    pred_class <- model$classes[which.max(log_probs)]
    predictions <- c(predictions, as.character(pred_class))
  }
  
  return(predictions)
}

# 🧠 4. Trainieren
gnb_model <- train_gnb(train_data, label_col = "Species")

# 🔍 5. Vorhersagen auf Testdaten
test_features <- test_data[, setdiff(names(test_data), "Species")]
test_labels <- test_data$Species

preds <- predict_gnb(gnb_model, test_features)

# 🎯 6. Accuracy ausgeben
accuracy <- mean(preds == test_labels)
cat("Accuracy:", accuracy, "\n")

# Optional: Konfusionsmatrix
table(Predicted = preds, Actual = test_labels)
