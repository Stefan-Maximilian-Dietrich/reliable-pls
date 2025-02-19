#Test e-admissible#
data_frame

labeled_data = data_frame[200:192,]
unlabeled_data = data_frame[11:90,]
test_data = data_frame[91:190,]
target = "target_var"
formula = target_var ~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6
prioris = normal_radnom_spaced(1000, c(-2,-2,-2,-2,-2,-2,-2), c(2,2,2,2,2,2,2))
likelihood = likelihood_logistic
alpha = 0.5

source("e_admissible.R")


Test <- e_admissible(labeled_data = labeled_data,
                     unlabeled_data = unlabeled_data,
                     test_data = test_data,
                     target = target,
                     glm_formula = formula,
                     prioris = prioris,
                     likelihood = likelihood,
                     alpha = alpha,
                     creterion = "maximal") 

################################################################################
set.seed(123)
n <- 100
num_features <- 3  # Kleinere Dimension für bessere Übersicht

# Zufällige Features erstellen
X <- as.data.frame(matrix(runif(n * num_features, -2, 2), nrow = n))
colnames(X) <- paste0("feature_", 1:num_features)

# Binäre Zielvariable generieren
beta_true <- runif(num_features, -1, 1)  # Zufällige "wahre" Koeffizienten
p <- 1 / (1 + exp(-as.matrix(X) %*% beta_true))  # Logistische Funktion
Y <- rbinom(n, 1, p)

# DataFrame mit Zielvariable
df <- data.frame(target = Y, X)

# Logistische Regression mit glm()
formula <- as.formula("target ~ feature_1 + feature_2 + feature_3")
glm_model <- glm(formula, data = df, family = binomial)

# Observed Fisher-Information aus glm() (negative inverse Kovarianzmatrix)
fisher_glm <- solve(vcov(glm_model))

print("Observed Fisher Information from glm():")
print(fisher_glm)



# Log-Likelihood-Funktion für logistische Regression
log_likelihood <- function(theta, X, Y) {
  p <- 1 / (1 + exp(-X %*% theta))
  return(-sum(Y * log(p) + (1 - Y) * log(1 - p)))  # Negative Log-Likelihood
}

# Design-Matrix mit Intercept
X_matrix <- cbind(1, as.matrix(X))

# Startwerte für Optimierung
theta_start <- rep(0, num_features + 1)

# Optimierung mit optim()
optim_result <- optim(
  par = theta_start,
  fn = log_likelihood,
  X = X_matrix,
  Y = Y,
  method = "BFGS",
  hessian = TRUE
)

# Observed Fisher-Information aus optim() (negative Hesse-Matrix)
fisher_optim <- optim_result$hessian

print("Observed Fisher Information from optim():")
print(fisher_optim)

# Differenz der beiden Fisher-Matrizen
diff_matrix <- fisher_glm + fisher_optim

print("Difference between glm() and optim() Fisher matrices:")
print(diff_matrix)

#####################################################
labeled_data = data_frame[1:10,]
unlabeled_data = data_frame[11:15,]
target = "target_var"
formula = target_var ~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6

logistic_modell_function(labeled_data, unlabeled_data, formula)[[1]](rep(100,7))


#################################################################################
ppp <- function(dims, priori, log_likelihood) {
  n_var <- dims[1]
  n_obs <- dims[2]
  
  
  otim <-  optim(rep(0,n_var), log_likelihood, control = list(fnscale = -1), hessian = TRUE)
  
  max <- otim$value
  argmax <- otim$par
  hessian <- otim$hessian
  print(hessian)
  
  print(paste("log: ", max, " priori: ", priori[[1]](argmax), " hessian: ", log(det(-hessian)), " rest:", n_var/2 * log(2*pi/n_obs) ))
  
  
  result <- max + n_var/2 * log(2*pi/n_obs)  + log(priori[[1]](argmax)) - 0.25 * log(det(-hessian))
  
  return(result)
}

prioris <-  normal_radnom_spaced(10, c(-2,-2,-2,-2,-2,-2,-2), c(2,2,2,2,2,2,2))

target = "target_var"
formula = target_var ~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6
labeled_data = data_frame[0:100,]
unlabeled_data = data_frame[101:105,]
ll <- logistic_modell_function(labeled_data, unlabeled_data, formula)

ppp(dims, prioris[[1]], ll[[1]]) 





dims <- c(7, 10)

optim(rep(0,7), ll[[9]], control = list(fnscale = -1), hessian = TRUE)$value
a <- optim(rep(0,7), ll[[1]], control = list(fnscale = -1), hessian = TRUE)$par


optim(rep(0,7), ll[[1]], control = list(fnscale = -1), hessian = TRUE)$par
optim(rep(100,7), ll[[1]], control = list(fnscale = -1), hessian = TRUE, method ="BFGS")$par
opt <- optim(rep(0,7), ll[[2]], control = list(fnscale = -1), hessian = TRUE)$par
optim(rep(0,7), ll[[3]], control = list(fnscale = -1), hessian = TRUE)$par

optim(true, ll[[1]], control = list(fnscale = -1), hessian = TRUE)$value
optim(rep(1,7), ll[[1]], control = list(fnscale = -1), hessian = TRUE)$value
optim(rep(0,7), ll[[2]], control = list(fnscale = -1), hessian = TRUE)$value
optim(rep(0,7), ll[[3]], control = list(fnscale = -1), hessian = TRUE)$value


ppp(dims, prioris[[1]], ll[[1]]) 
ll[[1]](c(0,0,0,0,0,0,0))
ll[[1]](a+0.5)


for(i in 1:1000) {
  mu_priori <- NULL
  for(j in 1:7) {
    mu_priori <- c(mu_priori, runif(1, min = -0.1, max =0.1))
  }
  
  value <- ll[[1]](mu_priori)
  print(value)

}

a <- data_frame[c(30:39, 41),]

ll[[1]](c(0,0,0,0,0,0,0))
ll[[2]](opt + c(45,0,0,0,0,0,0))
ll[[2]](opt)

X <- as.matrix(cbind(1, a[,-1]))

p <- 1 / (1 + exp(-X %*% opt))
log_likelihood <- sum(a[,1] * log(p+ 10^(-280)) + (1 - a[,1]) * log(1 - p + 10^(-280))) 


