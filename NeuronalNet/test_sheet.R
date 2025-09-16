set.seed(123)
data(iris)

# Indizes je Klasse


idx_by_class <- split(seq_len(nrow(iris)), iris$Species)

# 60% Train, 20% Test, 20% Unlabeled (pro Klasse)
split_per_class <- lapply(idx_by_class, function(ix) {
  n  <- length(ix)
  n_tr <- round(0.30 * n)
  tr <- sample(ix, n_tr)
  rest <- setdiff(ix, tr)
  n_te <- round(0.50 * length(rest))  # Hälfte des Rests -> Test
  te <- if (length(rest) > 0) sample(rest, n_te) else integer(0)
  ul <- setdiff(rest, te)             # verbleibend -> Unlabeled
  list(train = tr, test = te, unlabeled = ul)
})

train_idx     <- unlist(lapply(split_per_class, `[[`, "train"))
test_idx      <- unlist(lapply(split_per_class, `[[`, "test"))
unlabeled_idx <- unlist(lapply(split_per_class, `[[`, "unlabeled"))

train     <- iris[train_idx, ]
test      <- iris[test_idx, ]
unlabeled <- iris[unlabeled_idx, ]

## ===== Skalierung anhand Training =====
num_cols <- names(iris)[1:4]

train_x <- scale(train[, num_cols])
scale_center <- attr(train_x, "scaled:center")
scale_scale  <- attr(train_x, "scaled:scale")

test_x      <- scale(test[, num_cols],      center = scale_center, scale = scale_scale)
unlabeled_x <- scale(unlabeled[, num_cols], center = scale_center, scale = scale_scale)

train_scaled     <- data.frame(train_x,     Species = droplevels(train$Species))
test_scaled      <- data.frame(test_x,      Species = droplevels(test$Species))
unlabeled_scaled <- data.frame(unlabeled_x, Species = droplevels(unlabeled$Species))
## 1) Feste Architektur & Basisfunktionen
classes <- levels(train_scaled$Species)
d <- length(num_cols)   # 4 Inputs
h <- 5                  # Hidden Units (fix)
K <- length(classes)    # 3 Klassen

theta_length <- h*d + h + K*h + K  # W1(h*d) + b1(h) + W2(K*h) + b2(K)


priors <- list(
  list(ID = 1,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)), 
  list(ID = 2,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)), 
  list(ID = 3,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 4,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 5,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 6,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 7,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 8,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 9,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)),
  list(ID = 9,  mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1))
  
)



alpha = 0.5
a <- e_admissible_SSL(prioris, train_scaled, unlabeled_scaled, test_scaled, alpha) 







sample_three_df <- function(data, formula, size1, size2) {
  include_na = FALSE
  seed = NULL
  variables <- all.vars(formula) 
  x <- variables[1]
  df <- data[, variables]
  
  
  stopifnot(is.data.frame(df), x %in% names(df))
  if (!include_na) df <- df[!is.na(df[[x]]), , drop = FALSE]
  if (nrow(df) == 0L) stop("Keine Daten (nach NA-Filter).")
  if (!is.null(seed)) set.seed(seed)
  
  k <- length(unique(df[[x]]))
  if (size1 < k) stop(sprintf("size1 (%d) < Anzahl Kategorien (%d).", size1, k))
  if (size1 > nrow(df)) stop(sprintf("size1 (%d) > Zeilenanzahl (%d).", size1, nrow(df)))
  
  # --- Set 1: jede Kategorie mindestens 1x ---
  idx_one_each <- unlist(lapply(split(seq_len(nrow(df)), df[[x]]),
                                function(ix) sample(ix, 1L)))
  rest_needed <- size1 - length(idx_one_each)
  idx_rest1 <- if (rest_needed > 0L) {
    sample(setdiff(seq_len(nrow(df)), idx_one_each), rest_needed)
  } else integer(0)
  idx_set1 <- sample(c(idx_one_each, idx_rest1))
  
  # Rest nach Set 1
  remaining_after_1 <- setdiff(seq_len(nrow(df)), idx_set1)
  
  # --- Set 2: Zufall aus Rest ---
  if (size2 < 0) stop("size2 muss >= 0 sein.")
  if (size2 > length(remaining_after_1)) {
    stop(sprintf("size2 (%d) > verbleibende Zeilen nach Set 1 (%d).",
                 size2, length(remaining_after_1)))
  }
  idx_set2 <- if (size2 > 0L) sample(remaining_after_1, size2) else integer(0)
  
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

sample_three_df(data, formula, 8, 40)

# Kontrolle: in Set1+Set2 ~ z-standardisiert
colMeans(rbind(out$set1_cover[,1:4], out$set2_random[,1:4]))
apply(rbind(out$set1_cover[,1:4], out$set2_random[,1:4]), 2, sd)

# Kontrolle: in Set3 separat ~ z-standardisiert
colMeans(out$set3_rest[,1:4])
apply(out$set3_rest[,1:4], 2, sd)

