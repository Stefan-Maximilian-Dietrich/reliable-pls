model <- gaussian_naive_bayes(x = as.matrix(train[, -1]), y = as.factor(train$target), prior = as.numeric(priori))
posterior_probs <- predict(model, newdata = as.matrix(train[, -1]), type = "prob")
name <- colnames(posterior_probs)

# Beispiel: Manuelles Modell
# 1. Mittelwerte der Merkmale pro Klasse (Matrix)
means <- t(model$params$mu)
# 2. Standardabweichungen der Merkmale pro Klasse (Matrix)
sds <-t(model$params$sd)

# 3. Prior-Wahrscheinlichkeiten der Klassen
priors <-model$prior

# Neue Daten (Testdaten)
new_data <-as.matrix(train[, -1])
# Manuelle Berechnung der Wahrscheinlichkeiten
manual_predict <- function(means, sds, priors, new_data) {
  
  # Klassen (Spaltennamen der Mittelwertmatrix)
  classes <- colnames(means)
  
  # Ergebnis für jede neue Beobachtung (jede Zeile in new_data)
  result <- apply(new_data, 1, function(x) {
    
    # Berechne die Posterior-Wahrscheinlichkeit für jede Klasse
    posterior_probs <- sapply(classes, function(class) {
      
      # Berechne die Wahrscheinlichkeitsdichte jedes Merkmals in dieser Klasse
  
        # Mittelwert und Standardabweichung für das Merkmal i und Klasse
        mu <- means[, class]
        sigma <- sds[, class]
        
        # Berechne die Dichte der Normalverteilung für jedes Merkmal

        feature_probs <- dnorm(x, mean = mu, sd = sigma)
   
      
      # Multipliziere die Dichten und berücksichtige die Prior-Wahrscheinlichkeit der Klasse
      prior_prob <- priors[class]
      likelihood <- prod(feature_probs)
      posterior_prob <- prior_prob * likelihood
      return(posterior_prob)
    })
    
    # Normalisiere die Posterior-Wahrscheinlichkeiten, damit ihre Summe 1 ergibt
    total_prob <- sum(posterior_probs)
    posterior_probs_normalized <- posterior_probs / total_prob
    return(posterior_probs_normalized)
  })
  
  # Gebe die normalisierten Posterior-Wahrscheinlichkeiten zurück
  return(t(result))
}


# Zeige die resultierenden Wahrscheinlichkeiten



### benchmark 
profvis({
  
  for(i in 1:10000){
     manual_predict(means, sds, priors, new_data)
  }
  
})

profvis({
  for(i in 1:10000){
     predict(model, newdata = new_data, type = "prob")
  }
})

profvis({
  for(i in 1:100000){
    dnorm(1, mean = 0, sd = 2)
    mvnfast::dmvn(X = 1, mu =0, sigma = 2)   }
})