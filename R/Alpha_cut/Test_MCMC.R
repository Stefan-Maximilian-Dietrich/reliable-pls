n = 100
# Alpha cut 
mu_priori_lower <- c(-2, -10, -1, -1, -1, -1, -1)
mu_priori_upper <-  c(5, 0, 1, 1, 1, 1, 1) 
sigma_priori <- matrix(c(3,0,0,0,0,0,0,
                         0,4,0,0,0,0,0,
                         0,0,1,0,0,0,0,
                         0,0,0,1,0,0,0,
                         0,0,0,0,1,0,0,
                         0,0,0,0,0,1,0,
                         0,0,0,0,0,0,1), nrow = 7, byrow = TRUE)
alpha = 0.8

#simulate Data
feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = -2)
feature_3 <- rnorm(n, mean = 1.8)
feature_4 <- rnorm(n, mean = -1)
feature_5 <- rnorm(n, mean = 1.5, sd = 4)
feature_6 <- rnorm(n, mean = -1, sd = 8)
feature_7 <- rnorm(n, mean = -2, sd = 1)
feature_8 <- rnorm(n, mean = 0, sd = 4)
feature_9 <- rnorm(n, mean = 4, sd = 8)

lin_comb <- 2.4- 7.9*feature_1 + 0.5*feature_2

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)
sum(target_var)
data_frame <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2,
                         feature_3 = feature_3, feature_4 = feature_4, feature_5 = feature_5, feature_6 = feature_6) #, feature_7 = feature_7, feature_8 = feature_8,

formula <- target_var ~ .
glm(formula = formula, data = data_frame, family = "binomial") %>% summary()

data_frame = data_frame %>% as.data.frame()
name_df = "simulated" # for results 
data = "simulated" # for results 

#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)



# formula for logistic regression
formula = target_var~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6 #+



summary <- glm(formula = formula, data = data_frame[1:50,], family = "binomial") %>% summary()
summary
#p <- summary$coefficients %>% nrow() - 1

target = "target_var" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$target_var <- as.numeric(data_frame$target_var) -1

A <- c()
profvis({
  for(i in 1:10) {
  A <- c(A,gamma_maximin_alpaC_addapter(data = data_frame, glm_formula = formula, target = target, mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_upper, sigma_priori = sigma_priori, alpha = 0.8))
  }
})



