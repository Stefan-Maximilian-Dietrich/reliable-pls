evaluate_gnb_with_confusion <- function(train_data, test_data, class_col = "Species") {
  # Lade benötigte Pakete
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Das Paket 'caret' muss installiert sein.")
  }
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Das Paket 'e1071' muss installiert sein.")
  }
  
  # Features und Klassen trennen
  features <- setdiff(names(train_data), class_col)
  
  # Modell trainieren mit e1071::naiveBayes
  model <- e1071::naiveBayes(
    x = train_data[, features],
    y = as.factor(train_data[[class_col]])
  )
  
  # Vorhersagen für Testdaten
  predictions <- predict(model, newdata = test_data[, features])
  
  # Wahres Label extrahieren
  ground_truth <- as.factor(test_data[[class_col]])
  
  # Konfusionsmatrix berechnen
  cm <- caret::confusionMatrix(predictions, ground_truth)
  
  return(cm)
}

# 1. HELPER: Marginal Likelihood for a single feature
marginal_likelihood_feature <- function(x, mu0, kappa0, alpha0, beta0) {
  n <- length(x)
  x_bar <- mean(x)
  s2 <- var(x)
  
  kappa_n <- kappa0 + n
  alpha_n <- alpha0 + n / 2
  beta_n <- beta0 + 0.5 * (n - 1) * s2 + 
    (kappa0 * n * (x_bar - mu0)^2) / (2 * (kappa0 + n))
  
  const <- sqrt(kappa0 / kappa_n) * 
    gamma(alpha_n) / gamma(alpha0) * 
    (beta0^alpha0) / (beta_n^alpha_n)
  
  logml <- log(const) - (n / 2) * log(2 * pi)
  return(logml)
}

# 2. FUNCTION: Prior Sampler
sample_priors <- function(n = 3, d = 4) {
  priors <- vector("list", n)
  for (i in seq_len(n)) {
    priors[[i]] <- list(
      id = paste0("Prior_", i),
      mu0 = rnorm(d, mean = 0, sd = 2),       # zufälliges Zentrum
      kappa0 = runif(1, 0.5, 5),              # Streuung des Prior-Wissens
      alpha0 = runif(1, 1.5, 5),              # Formparameter (Varianz)
      beta0 = runif(1, 1.0, 10)               # Streuung der Varianz
    )
  }
  return(priors)
}
# 3. FUNCTION: Compute marginal likelihood for full dataset under 1 prior
compute_gnb_marginal_likelihood <- function(data, class_col, prior) {
  features <- setdiff(names(data), class_col)
  classes <- unique(data[[class_col]])
  total_logml <- 0
  
  for (class in classes) {
    subset <- data[data[[class_col]] == class, ]
    
    class_logml <- 0
    for (feature in features) {
      x <- subset[[feature]]
      logml <- marginal_likelihood_feature(
        x,
        mu0 = prior$mu0,
        kappa0 = prior$kappa0,
        alpha0 = prior$alpha0,
        beta0 = prior$beta0
      )
      class_logml <- class_logml + logml
    }
    
    total_logml <- total_logml + class_logml
  }
  
  return(total_logml)
}

# 4. FUNCTION: Run over all prior samples
evaluate_priors <- function(data, class_col, priors) {
  results <- data.frame(Prior = character(), LogMarginalLikelihood = numeric())
  
  for (prior in priors) {
    logml <- compute_gnb_marginal_likelihood(data, class_col, prior)
    results <- rbind(results, data.frame(Prior = prior$id, LogMarginalLikelihood = logml))
  }
  
  return(results)
}

alpha_cut_catteneo <- function(evidence_df, priors, alpha = 0.9) {
  if (!("LogMarginalLikelihood" %in% names(evidence_df))) {
    stop("evidence_df muss eine Spalte 'LogMarginalLikelihood' enthalten.")
  }
  
  # Finde maximale log-Likelihood (Referenz)
  max_logml <- max(evidence_df$LogMarginalLikelihood)
  
  # Berechne Memberships (skalierte Evidenz)
  evidence_df$membership <- exp(evidence_df$LogMarginalLikelihood - max_logml)
  
  # Wähle Prioris mit membership >= α
  passed <- evidence_df$membership >= alpha
  selected_ids <- evidence_df$Prior[passed]
  
  # Hole zugehörige Priorparameter
  selected_priors <- Filter(function(p) p$id %in% selected_ids, priors)
  
  return(selected_priors)
}

pseudolabeled <- function(train_data, unlabeled_data, class_col = "Species") {
  # Pakete prüfen
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Das Paket 'e1071' muss installiert sein.")
  }
  
  # Features bestimmen
  features <- setdiff(names(train_data), class_col)
  
  # GNB-Modell trainieren
  model <- e1071::naiveBayes(
    x = train_data[, features],
    y = as.factor(train_data[[class_col]])
  )
  
  # Sicherstellen, dass das Label im Unlabeled-Set entfernt wird
  unlabeled_features <- unlabeled_data[, features, drop = FALSE]
  
  # Vorhersagen
  pseudo_labels <- predict(model, newdata = unlabeled_features)
  
  # Rückgabe: originaler Datensatz mit denselben Features + Labelspalte
  pseudolabeled_data <- unlabeled_features
  pseudolabeled_data[[class_col]] <- pseudo_labels
  
  return(pseudolabeled_data)
}

expand_with_pseudolabels <- function(train_data, pseudo_data) {
  # Anzahl der Pseudo-Instanzen
  n <- nrow(pseudo_data)
  
  # Liste initialisieren
  dataset_list <- vector("list", length = n)
  
  for (i in seq_len(n)) {
    new_point <- pseudo_data[i, , drop = FALSE]
    dataset_list[[i]] <- rbind(train_data, new_point)
  }
  
  return(dataset_list)
}

compute_exact_ppp_point <- function(train_data, x_point, class_col, prior) {
  # Stelle sicher, dass das Label im Pseudopunkt vorhanden ist
  label <- as.character(x_point[[class_col]])
  features <- setdiff(names(train_data), class_col)
  
  # Daten für die Klasse y extrahieren
  class_data <- train_data[train_data[[class_col]] == label, features, drop = FALSE]
  n <- nrow(class_data)
  
  # Posterior-Parameter für jede Feature-Spalte
  mu0 <- prior$mu0
  kappa0 <- prior$kappa0
  alpha0 <- prior$alpha0
  beta0 <- prior$beta0
  
  log_ppp <- 0
  
  for (j in seq_along(features)) {
    xj <- x_point[[features[j]]]
    data_j <- class_data[[j]]
    
    # Posteriorparameter
    x_bar <- mean(data_j)
    s2 <- var(data_j)
    
    kappa_n <- kappa0 + n
    mu_n <- (kappa0 * mu0[j] + n * x_bar) / kappa_n
    alpha_n <- alpha0 + n / 2
    beta_n <- beta0 + 0.5 * (n - 1) * s2 +
      (kappa0 * n * (x_bar - mu0[j])^2) / (2 * kappa_n)
    
    # Student-t Parameter
    nu <- 2 * alpha_n
    scale2 <- beta_n * (1 + 1 / kappa_n) / alpha_n
    sd <- sqrt(scale2)
    
    # log PDF der Student-t Verteilung
    log_density <- dt((xj - mu_n) / sd, df = nu, log = TRUE) - log(sd)
    
    log_ppp <- log_ppp + log_density
  }
  
  return(log_ppp)
}

compute_exact_ppp_matrix <- function(train_data, pseudo_data, priors, class_col = "Species") {
  n_pseudo <- nrow(pseudo_data)
  n_priors <- length(priors)
  
  result <- matrix(NA, nrow = n_pseudo, ncol = n_priors)
  rownames(result) <- paste0("Pseudo_", seq_len(n_pseudo))
  colnames(result) <- sapply(priors, function(p) p$id)
  
  for (j in seq_len(n_priors)) {
    prior <- priors[[j]]
    
    for (i in seq_len(n_pseudo)) {
      x_point <- pseudo_data[i, , drop = FALSE]
      result[i, j] <- compute_exact_ppp_point(train_data, x_point, class_col, prior)
    }
  }
  
  return(result)
}


####

set.seed(42)
train_index <- sample(1:nrow(iris), 100)
unlabeled_data <- iris[-train_index, ]
unlabeled_data$Species <- NULL


# Pseudo-Labeling
train_data <- iris[train_index, ]

pseudo_data <- pseudolabeled(train_data, unlabeled_data, class_col = "Species")
priors <- sample_priors()

sample_priors <- function(n = 3, d = 4) {
  priors <- vector("list", n)
  for (i in seq_len(n)) {
    priors[[i]] <- list(
      id = paste0("Prior_", i),
      mu0 = rnorm(d, mean = 0, sd = 2),       # zufälliges Zentrum
      kappa0 = runif(1, 0.5, 5),              # Streuung des Prior-Wissens
      alpha0 = runif(1, 1.5, 5),              # Formparameter (Varianz)
      beta0 = runif(1, 1.0, 10)               # Streuung der Varianz
    )
  }
  return(priors)
}

compute_exact_ppp_matrix(train_data, pseudo_data, priors, class_col = "Species") 

rows_equal <- function(mat) {
  apply(mat, 1, function(row) length(unique(row)) == 1)
}
rows_equal(m)
test_compute_exact_ppp_matrix <- function() {
  
  cat("🧪 TEST: compute_exact_ppp_matrix\n")
  
  # Schritt 1: Daten vorbereiten
  set.seed(123)
  train_index <- sample(1:nrow(iris), 100)
  train_data <- iris[train_index, ]
  unlabeled_data <- iris[-train_index, ]
  unlabeled_data$Species <- NULL
  
  # Schritt 2: Pseudo-Labeling
  pseudo_data <- pseudolabeled(train_data, unlabeled_data, class_col = "Species")
  
  # Schritt 3: Prior-Liste erzeugen
  sample_priors <- function(n = 3) {
    priors <- vector("list", n)
    for (i in seq_len(n)) {
      priors[[i]] <- list(
        id = paste0("Prior_", i),
        mu0 = rep(0, 4),        # Iris hat 4 Features
        kappa0 = 1.0,
        alpha0 = 2.0,
        beta0 = 2.0
      )
    }
    return(priors)
  }
  priors <- sample_priors(n = 3)
  
  # Schritt 4: PPP-Matrix berechnen
    m <- compute_exact_ppp_matrix(train_data, pseudo_data, priors, class_col = "Species")
  
  # Schritt 5: Prüfen
  if (!is.matrix(ppp_mat)) {
    cat("❌ Fehler: Ergebnis ist keine Matrix\n")
  } else if (any(is.na(ppp_mat))) {
    cat("⚠️ Warnung: Matrix enthält NAs\n")
  } else if (!all(dim(ppp_mat) == c(nrow(pseudo_data), length(priors)))) {
    cat("❌ Fehler: Matrixdimensionen sind falsch\n")
  } else {
    cat("✅ Erfolgreich: PPP-Matrix mit", nrow(ppp_mat), "Pseudozeilen und", ncol(ppp_mat), "Priors\n")
    print(round(ppp_mat[1:5, , drop = FALSE], 4))  # nur ersten 5 Zeilen anzeigen
  }
}

test_compute_exact_ppp_matrix()

test_compute_exact_ppp_point <- function() {
  cat("🧪 TEST: compute_exact_ppp_point\n")
  
  # Schritt 1: Trainingsdaten erzeugen
  set.seed(123)
  train_index <- sample(1:nrow(iris), 100)
  train_data <- iris[train_index, ]
  
  # Schritt 2: Ein gültiger Pseudopunkt mit Label
  pseudo_point <- iris[-train_index, ][1, , drop = FALSE]
  
  # Schritt 3: Beispielhafter gültiger konjugierter Prior
  prior <- list(
    id = "TestPrior",
    mu0 = rep(0, 4),       # Iris hat 4 Features
    kappa0 = 1.0,
    alpha0 = 2.0,
    beta0 = 2.0
  )
  
  # Schritt 4: Funktion aufrufen
  log_ppp <- compute_exact_ppp_point(
    train_data = train_data,
    x_point = pseudo_point,
    class_col = "Species",
    prior = prior
  )
  
  # Schritt 5: Testergebnis prüfen
  if (is.na(log_ppp)) {
    cat("❌ Fehler: PPP ist NA\n")
  } else if (!is.numeric(log_ppp)) {
    cat("❌ Fehler: PPP ist kein numerischer Wert\n")
  } else {
    cat("✅ Erfolgreich: Log PPP =", round(log_ppp, 4), "\n")
  }
}
test_compute_exact_ppp_point()

# Priors generieren
priors <- sample_priors(n = 3)

# Exakte PPP-Matrix berechnen
ppp_matrix <- compute_exact_ppp_matrix(train_data, pseudo_data, priors, class_col = "Species")
# Pseudo-Labeling
pseudo_data <- pseudolabeled(train_data, unlabeled_data, class_col = "Species")

# Priors generieren
priors <- sample_priors(n = 3)

# Exakte PPP-Matrix berechnen
ppp_matrix <- compute_exact_ppp_matrix(train_data, pseudo_data, priors, class_col = "Species")
print(round(ppp_matrix, 4))

#####
# Beispiel mit Iris
set.seed(42)
train_index <- sample(1:nrow(iris), size = 0.7 * nrow(iris))
train_data <- iris[train_index, ]
unlabeled_data <- iris[-train_index, ]

# Simuliere: entferne "Species" (Label)
unlabeled_data$Species <- NULL

# Pseudo-Labeling durchführen
pseudo_data <- pseudolabeled(train_data, unlabeled_data, class_col = "Species")

# Struktur prüfen
head(pseudo_data)
str(pseudo_data)

# 5. TEST SHEET / DEMO
priors <- sample_priors(n = 1000)
evidence_df <- evaluate_priors(iris, class_col = "Species", priors)
alpha_result <- alpha_cut_catteneo(evidence_df, priors, alpha = 0.1)

# Pseudo Posterior predictive 
# Pakete laden
library(caret)
library(e1071)

# Iris zufällig in Trainings- und Testdaten aufteilen
set.seed(123)
train_index <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# Funktion aufrufen
cm_result <- evaluate_gnb_with_confusion(train_data, test_data, class_col = "Species")


print("α-Cut Schwelle:")
print(alpha_result$threshold)

print("Ausgewählte Prior-IDs:")
print(alpha_result$selected_ids)

print("Ausgewählte Priorparameter:")
print(alpha_result$selected_priors)
