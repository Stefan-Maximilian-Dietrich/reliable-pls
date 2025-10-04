Errors <- readRDS("~/Bachelorarbeit/Errors.rds")
View(Errors)
Last <- readRDS("~/Bachelorarbeit/Last.rds")
View(Last)

i <- 1
gamma_maximin_alpaC_addapter(data = Errors[[i]]$data, glm_formula = Errors[[i]]$glm_formula, target = Errors[[i]]$target, mu_priori_lower = Errors[[i]]$mu_priori_lower, mu_priori_upper = Errors[[i]]$mu_priori_upper, sigma_priori = Errors[[i]]$sigma_priori, alpha = Errors[[i]]$alpha)
  
