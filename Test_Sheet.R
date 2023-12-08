share_unlabeled = 0.8
set.seed(2138720)

# simulate data
p = 6 
n = 200
N = 40

feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = -2)
feature_3 <- rnorm(n, mean = 1.8)
feature_4 <- rnorm(n, mean = -1)
feature_5 <- rnorm(n, mean = 1.5, sd = 4)
feature_6 <- rnorm(n, mean = -1, sd = 8)

lin_comb <- 2.4- 7.9*feature_1 + 0.5*feature_2

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)
sum(target_var)
data_frame <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2,
                         feature_3 = feature_3, feature_4 = feature_4, feature_5 = feature_5, feature_6 = feature_6) 

data_frame = data_frame %>% as.data.frame()
name_df = "simulated" # for results 
data = "simulated" # for results 

#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)