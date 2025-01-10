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