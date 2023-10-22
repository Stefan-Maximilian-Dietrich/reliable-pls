#Test der Aproxximation
# berechne m Tensor für alle 
Limits <- matrix(c(-5,5,1,
                   -5,5,1,
                   -5,5,1,
                   -5,5,1), ncol = 3, byrow = TRUE)

x1 <- rnorm(10000, mean = 0, sd = 1)
x2 <- rnorm(10000, mean = 2, sd = 5)
x3 <- rnorm(10000, mean = 1, sd = 2)

lin_comb <- -0.5 - 1*x1 +  2.8*x2 + 0.4*x3

prob = 1/(1+exp(-lin_comb))

y <-rbinom(10000, 1, prob = prob)


df <- as.data.frame(cbind(y, x1, x2, x3))
data100_1 <- df[2:101, ]
data100_2 <- df[1:1000, ]

sigma_priori_1  <- rbind(beta0 = c(1,0,0,0),cbind(beta0 = c(0,0,0), cov(data100_1[c(2,3,4)]) ))
sigma_priori_2  <- rbind(beta0 = c(1,0,0,0),cbind(beta0 = c(0,0,0), cov(data100_2[c(2,3,4)]) ))

formula <- as.formula( y ~ x1 + x2 + x3)

logistic_model <- glm(formula = formula, 
                      data = data100_2, 
                      family = "binomial")
summary(logistic_model)

profvis({
  e <- gamma_maximin_alpaC_addapter(data = data100_1, glm_formula = formula, target = "y", mu_priori_lower = c(-5,-5,-5,-5), mu_priori_upper = c(5,5,5,5), sigma_priori = sigma_priori_1, alpha = 0.8)
  e
})


start <- Sys.time()
f <- gamma_maximin_alpaC_addapter(data = data100_2, glm_formula = formula, target = "y", mu_priori_lower = c(-5,-5,-5,-5), mu_priori_upper = c(5,5,5,5), sigma_priori = sigma_priori_2, alpha = 0.8) 
end <- Sys.time()
end - start


################################################################################
#Banknoten
share_unlabeled = 0.8

# read in data frame
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

#get number of instances
data_frame = data_frame[sample(nrow(data_frame), 80),]


#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "banknote" # for results 
data = "banknote"
# formula for glm
formula = target ~  Diagonal+ Bottom + Length #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$target <- as.numeric(data_frame$target) - 1 # Musste uch hinzufügen 


glm_1 <- glm(formula = formula, data = data_frame, family = "binomial")

n_imp = ((nrow(data_frame) - n_test) * share_unlabeled) %>% round()
ind_res_on_the_fly = matrix(nrow = n_imp, ncol = N)

