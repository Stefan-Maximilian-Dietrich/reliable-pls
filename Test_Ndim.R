set.seed(33456456)

x1 <- rnorm(10000, mean = 0, sd = 1)
x2 <- rnorm(10000, mean = 2, sd = 5)
x3 <- rnorm(10000, mean = 1, sd = 2)

lin_comb <- -8.4 - 7.9*x1 +  2.8*x2 + 0.4*x3

prob = 1/(1+exp(-lin_comb))

y <-rbinom(10000, 1, prob = prob)

df <- as.data.frame(cbind(y, x1, x2, x3))

formula <- formula(y ~ x1 + x2 + x3)

data100 <- df[1:100, ]
data100_u <- df[101:200, c(2,3,4)]
data500_t <- df[201:700, ]
data2_u <- df[101:102, c(2,3,4)]

logistic_model <- glm(formula = formula, data = data100, family = "binomial")
summary(logistic_model)

cov(data100[c(2,3,4)])

mu_priori_lower <- c(-15, -15, -5, -5)
mu_priori_upper <- c(5, 5, 10, 5)
sigma_priori  <- rbind(beta0 = c(1,0,0,0),cbind(beta0 = c(0,0,0), cov(data100[c(2,3,4)]) ))
alpha <- 0.7


alpha_cut(labeled_data = data100, unlabeled_data = data2_u, test_data = data500_t, target =  "y", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = alpha)

