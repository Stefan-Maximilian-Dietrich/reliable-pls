sample_NB <- function(data, formula, n_labled, n_unlabled) {
  include_na = FALSE
  seed = NULL
  variables <- all.vars(formula) 
  x <- variables[1]
  df <- data[, variables]
  
  
  stopifnot(is.data.frame(df), x %in% names(df))
  
  if (!include_na) df <- df[!is.na(df[[x]]), , drop = FALSE]
  if (nrow(df) == 0L) stop("Keine Daten (nach NA-Filter).")
  if (!is.null(seed)) set.seed(seed)
  
  # --- Set 1: jede Kategorie mindestens 1x ---
  idx_one_each <- unlist(lapply(split(seq_len(nrow(df)), df[[x]]), function(ix) sample(ix, 1L)))
  
  rest_needed <- n_labled - length(idx_one_each)
  
  idx_rest1 <- if (rest_needed > 0L) {
    sample(setdiff(seq_len(nrow(df)), idx_one_each), rest_needed)
  } else integer(0)
  idx_set1 <- sample(c(idx_one_each, idx_rest1))
  
  # Rest nach Set 1
  remaining_after_1 <- setdiff(seq_len(nrow(df)), idx_set1)
  
  # --- Set 2: Zufall aus Rest ---
  if (n_unlabled < 0) stop("n_unlabled muss >= 0 sein.")
  if (n_unlabled > length(remaining_after_1)) {
    stop(sprintf("n_unlabled (%d) > verbleibende Zeilen nach Set 1 (%d).",
                 n_unlabled, length(remaining_after_1)))
  }
  idx_set2 <- if (n_unlabled > 0L) sample(remaining_after_1, n_unlabled) else integer(0)
  
  # --- Set 3: alle übrigen ---
  idx_set3 <- setdiff(remaining_after_1, idx_set2)
  
  set1 <- df[idx_set1, , drop = FALSE]
  set2 <- df[idx_set2, , drop = FALSE]
  set3 <- df[idx_set3, , drop = FALSE]
  
  # --------- Standardisierung ---------
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    # Gemeinsame Stats für Set1+Set2
    comb <- rbind(set1[num_cols], set2[num_cols])
    m12 <- vapply(comb, mean, numeric(1), na.rm = TRUE)
    s12 <- vapply(comb, sd,   numeric(1), na.rm = TRUE)
    s12[s12 == 0 | is.na(s12)] <- 1
    
    scale_fun <- function(d, m, s) {
      for (nm in names(m)) d[[nm]] <- (d[[nm]] - m[[nm]]) / s[[nm]]
      d
    }
    
    set1 <- scale_fun(set1, m12, s12)
    set2 <- scale_fun(set2, m12, s12)
    
    # Separate Stats für Set3
    m3 <- vapply(set3[num_cols], mean, numeric(1), na.rm = TRUE)
    s3 <- vapply(set3[num_cols], sd,   numeric(1), na.rm = TRUE)
    s3[s3 == 0 | is.na(s3)] <- 1
    set3 <- scale_fun(set3, m3, s3)
  }
  
  list(
    set1_cover = set1,
    set2_random = set2,
    set3_rest   = set3
  )
}

evaluate_gnb_with_confusion <- function(train_data, test_data, class_col ) {
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

sample_priors <- function(n , d ) {
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

evaluate_priors <- function(data, class_col, priors) {
  results <- data.frame(Prior = character(), LogMarginalLikelihood = numeric())
  
  for (prior in priors) {
    logml <- compute_gnb_marginal_likelihood(data, class_col, prior)
    results <- rbind(results, data.frame(Prior = prior$id, LogMarginalLikelihood = logml))
  }
  
  return(results)
}

alpha_cut_catteneo <- function(evidence_df, priors, alpha ) {
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

pseudolabeled <- function(train_data, unlabeled_data, class_col) {
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

compute_exact_ppp_matrix <- function(train_data, pseudo_data, priors, class_col ) {
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
