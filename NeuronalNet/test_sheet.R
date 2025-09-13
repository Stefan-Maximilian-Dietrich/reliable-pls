set.seed(123)
data(iris)

# Indizes je Klasse
idx_by_class <- split(seq_len(nrow(iris)), iris$Species)

# 60% Train, 20% Test, 20% Unlabeled (pro Klasse)
split_per_class <- lapply(idx_by_class, function(ix) {
  n  <- length(ix)
  n_tr <- round(0.60 * n)
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
  list(ID = 10, mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1)), 
  list(ID = 11, mu = rnorm(theta_length, mean = 0, sd = 1),  tau2 = rexp(1, rate = 1))
)



alpha = 0.5
e_admissible_SSL(prioris, train_scaled, unlabeled_scaled, test_scaled, alpha) 