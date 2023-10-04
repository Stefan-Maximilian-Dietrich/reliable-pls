#Test Banknoten
set.seed(2037420)
formula = target ~  Diagonal + Bottom + Length

data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'
data_frame$target <- as.numeric(data_frame$target == "genuine")

modell <- glm(formula = target ~  Diagonal + Bottom + Length, data = data_frame)
summary(modell)

data_frame_160 = data_frame[sample(nrow(data_frame), 160),]
test_160 <- anti_join(data_frame, data_frame_160)
unlabeld_160 <- data_frame_160[sample(nrow(data_frame_160), round(160*0.8)),]
labled_160 <- anti_join(data_frame_160, unlabeld_160)


data_frame_120 = data_frame[sample(nrow(data_frame), 120),]
test_120 <- anti_join(data_frame, data_frame_120)
unlabeld_120 <- data_frame_120[sample(nrow(data_frame_120), round(120*0.8)),]
labled_120 <- anti_join(data_frame_120, unlabeld_120)

data_frame_80 = data_frame[sample(nrow(data_frame), 80),]
test_80 <- anti_join(data_frame, data_frame_80)
unlabeld_80 <- data_frame_160[sample(nrow(data_frame_80), round(80*0.8)),]
labled_80 <- anti_join(data_frame_80, unlabeld_80)

data_frame_40 = data_frame[sample(nrow(data_frame), 40),]
test_40 <- anti_join(data_frame, data_frame_40)
unlabeld_40 <- data_frame_40[sample(nrow(data_frame_40), round(40*0.8)),]
labled_40 <- anti_join(data_frame_40, unlabeld_40)

mu_priori_lower <- c(-45, -1, -1, -1)
mu_priori_lower <- c(-20, 1, 1, 1)
sigma_priori <-  rbind(beta0 = c(7,0,0,0),cbind(beta0 = c(0,0,0), cov(data_frame[c(7,5,2)]) ))
alpha = 0.8




start <- Sys.time()
Test_banknoten_40 <- alpha_cut(labeled_data = labled_40[c(1,7,5,2)], unlabeled_data = unlabeld_40[c(7,5,2)], test_data = test_40[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha)
End <- Sys.time()
End - start

start <- Sys.time()
Test_banknoten_40 <- alpha_cut(labeled_data = labled_80[c(1,7,5,2)], unlabeled_data = unlabeld_80[c(7,5,2)], test_data = test_80[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha)
End <- Sys.time()
End - start

start <- Sys.time()
Test_banknoten_120 <- alpha_cut(labeled_data = labled_120[c(1,7,5,2)], unlabeled_data = unlabeld_120[c(7,5,2)], test_data = test_120[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha)
End <- Sys.time()
End - start

start <- Sys.time()
Test_banknoten_160 <- alpha_cut(labeled_data = labled_160[c(1,7,5,2)], unlabeled_data = unlabeld_160[c(7,5,2)], test_data = test_160[c(1,7,5,2)], target =  "target", glm_formula = formula , mu_priori_lower = mu_priori_lower, mu_priori_upper = mu_priori_lower, sigma_priori = sigma_priori, alpha = alpha)
End <- Sys.time()
End - start