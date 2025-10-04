

###################
set.seed(1)

A <- M_MaxiMin_SSL(priors, data_train, data_unlabeled, data_test, alpha)
A[[1]]$overall["Accuracy"]
A[[81]]$overall["Accuracy"]

accuracies <- list()
for(i in 1:100) {
  set.seed(i)
  priors <- generate_random_priors(data_train, n_priors = 100,  kappa_range = c(0.1, 2), scale_range = c(0.5, 2),  nu_extra = c(0, 5, 10))
  data_sampled <- sampler_NB_up(4, 100, data, formula)
  data_train <- data_sampled[[1]]
  data_unlabeled <- data_sampled[[2]]
  data_test <- data_sampled[[3]]
  alpha <- 0.0001
  A <- M_MaxiMin_SSL(priors, data_train, data_unlabeled, data_test, alpha)
  accuracies[[i]] <- sapply(A, function(cm) cm$overall["Accuracy"])
  print(i/100)
}
mat <- do.call(cbind, accuracies)
plot(rowMeans(mat))
# Trainings- und Testsplit

iris_qda <- iris
names(iris_qda)[5] <- "target"

# Train/Test Split 50:50
idx <- sample(1:nrow(iris_qda), size = nrow(iris_qda)/2)
train <- iris_qda[idx, ]
test  <- iris_qda[-idx, ]

test_confiusion(train, test)


# ========== Priors definieren ==========
priors <- list(
  list(mu0 = c(5.5, 3.0), kappa0 = 1,   Lambda0 = diag(2),         nu0 = 5),
  list(mu0 = c(5.8, 3.0), kappa0 = 2,   Lambda0 = 2 * diag(2),     nu0 = 6),
  list(mu0 = c(5.0, 3.2), kappa0 = 0.5, Lambda0 = 0.5 * diag(2),   nu0 = 4),
  list(mu0 = c(5.5, 3.0), kappa0 = 1.5, Lambda0 = diag(c(1, 2)),   nu0 = 8),
  list(mu0 = c(5.5, 12.0), kappa0 = 1.5, Lambda0 = diag(c(1, 2)),   nu0 = 8)
  
)

marginal_likelihoods(data_train, priors)
# ========== Marginale Likelihoods berechnen ==========

# ========== Ausgabe ==========
results_df <- do.call(rbind, results)
print(results_df)
