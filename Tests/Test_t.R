profvis({
  e <- gamma_maximin_alpaC_addapter(data = data100_1, glm_formula = formula, target = "y", mu_priori_lower = c(-5,-5,-5,-5), mu_priori_upper = c(5,5,5,5), sigma_priori = sigma_priori_1, alpha = 0.8)
  e
})