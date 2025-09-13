## ===== Dreiteilung: Train / Test / Unlabeled (stratifiziert) =====
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

theta_test <- rnorm(43)
likelihood(theta_test,train_scaled )


######
priors <- list(
  # tendenziell "gut": breit oder gut zentriert
  list(ID = 1,          mu = rep(0, theta_length),           tau2 = 10),
  list(ID = 2,      mu = rep(0, theta_length),           tau2 = 1),
  list(ID = 3, mu = theta_pre_map,                  tau2 = 0.05),
  list(ID = 4, mu = theta_pre_map,               tau2 = 0.5),
  
  # tendenziell "schlecht": sehr eng oder falsches Zentrum
  list(ID = 5,   mu = rep(0, theta_length),           tau2 = 0.0025),
  list(ID = 6, mu = mu_wrong_1,                     tau2 = 0.05),
  list(ID = 7, mu = mu_wrong_2,                  tau2 = 0.5)
)

get_marginal_likelihood("first test", mu = rep(0, 43), tau2 = 1, theta_init = rep(10, 43), data_scaled_train = train_scaled, data_scaled_test = NULL, control = list(maxit = 1000, reltol = 1e-8)) 

theta_hat <- gradient_decent(train_scaled)


PPP_matrix <- function(priors, train_scaled, pseudolabeled_scaled) 