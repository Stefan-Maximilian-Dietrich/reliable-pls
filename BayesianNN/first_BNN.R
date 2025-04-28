install.packages("brms")
library(brms)


library(brms)

# Neuronales Netz manuell als nichtlineares Modell
# 2 Inputs → 2 Hidden Neuronen → Output
nn_formula <- bf(
  y ~ inv_logit(b3 +
                  w31 * tanh(b1 + w11*x1 + w12*x2) +
                  w32 * tanh(b2 + w21*x1 + w22*x2)),
  
  b1 + b2 + b3 + 
    w11 + w12 + w21 + w22 + w31 + w32 ~ 1,  # Alle Parameter sind frei
  nl = TRUE
)

# Prior-Verteilungen für die Gewichte
nn_priors <- c(
  prior(normal(0, 1), nlpar = "b1"),
  prior(normal(0, 1), nlpar = "b2"),
  prior(normal(0, 1), nlpar = "b3"),
  prior(normal(0, 1), nlpar = "w11"),
  prior(normal(0, 1), nlpar = "w12"),
  prior(normal(0, 1), nlpar = "w21"),
  prior(normal(0, 1), nlpar = "w22"),
  prior(normal(0, 1), nlpar = "w31"),
  prior(normal(0, 1), nlpar = "w32")
)
xor_data <- data.frame(
  x1 = c(0, 0, 1, 1),
  x2 = c(0, 1, 0, 1),
  y  = c(0, 1, 1, 0)
)
# Modell fitten
fit <- brm(
  formula = nn_formula,
  data = xor_data,
  family = bernoulli(),
  prior = nn_priors,
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 123
)

# Ergebnisse anschauen
summary(fit)
preds <- fitted(fit, newdata = xor_data, summary = TRUE)
preds
