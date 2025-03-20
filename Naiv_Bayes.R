
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'


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


y <- data_frame$target
X <- data_frame[, c(2, 5, 7)]
# Normalverteilte Features für jede Klasse

# In Dataframe umwandeln
data <- data.frame(y = factor(y), X)


# === 2. Gaussian Naïve Bayes mit flexiblem Prior p ===

n <- nrow(X)
train_idx <- sample(1:n, size = 0.03 * n)  # 80% Training
remaining_idx <- setdiff(1:n, train_idx)

val_idx <- sample(remaining_idx, size = 0.5 * length(remaining_idx))  # 10% Validierung
test_idx <- setdiff(remaining_idx, val_idx)  # 10% Test

# Datensätze erstellen
X_train <- X[train_idx, ]
y_train <- y[train_idx]
y_train

X_unl <- X[val_idx, ]
y_val <- y[val_idx]

X_test <- X[test_idx, ]
y_test <- y[test_idx]

####### Functions
train_gnb <- function(X, y, prior_p) {
  model <- list()
  
  # Definierte Prioris
  prior_0 <- 1 - prior_p  # P(y = 0)
  prior_1 <- prior_p      # P(y = 1)
  
  # Parameter für Klasse 0
  X_0 <- X[y == 0, ]
  model[["0"]] <- list(
    prior = prior_0,  
    mean = colMeans(X_0),  
    sd = apply(X_0, 2, sd)  
  )
  
  # Parameter für Klasse 1
  X_1 <- X[y == 1, ]
  model[["1"]] <- list(
    prior = prior_1,  
    mean = colMeans(X_1),  
    sd = apply(X_1, 2, sd)  
  )
  
  return(model)
}

likelihood <- function(model, X) {
  probs <- sapply(names(model), function(c) {
    prior <- model[[c]]$prior
    mean <- model[[c]]$mean
    sd <- model[[c]]$sd
    
    likelihood <- apply(X, 1, function(x) {
      prod(dnorm(x, mean = mean, sd = sd))  # Gauß-Wahrscheinlichkeiten
    })
    
    return(prior * likelihood)
  })
  
  log_likeli <- sum(log(apply(probs, 1, max)))   # Klasse mit max. Wahrscheinlichkeit
  return(log_likeli)
}

predict_gnb <- function(model, X) {
  probs <- sapply(names(model), function(c) {
    prior <- model[[c]]$prior
    mean <- model[[c]]$mean
    sd <- model[[c]]$sd
    
    likelihood <- apply(X, 1, function(x) {
      prod(dnorm(x, mean = mean, sd = sd))  # Gauß-Wahrscheinlichkeiten
    })
    
    return(prior * likelihood)
  })
  
  predicted_classes <- apply(probs, 1, which.max) - 1  # Klasse mit max. Wahrscheinlichkeit
  return(predicted_classes)
}

####### Alpha Cut 
result <- NULL
X <- X_train
Y <- y_train
start <- nrow(X_unl)
z = 0
while(z < (start - 1)) {
  
  
  prioris <- seq(0.1,0.9,0.005)
  res <- NULL
  for(i in prioris) {
    modell <- train_gnb(X,Y, i)
    a<- likelihood(modell, X)
    res <- rbind(res, c(i, exp(a)))
  }
  
  #plot(res)
  max <- max(res[,2])
  max_priori <-res[res[,2] == max, 1][1]
  alpha <- 0.9
  prioris_cut <- res[res[,2] >= max * alpha, 1]
  ######### Pseudo 
  modell <- train_gnb(X,Y, max_priori)
  y_pseudo <- predict_gnb(modell, X_unl)
  
  
  
  #### Matrix 
  k <- nrow(X_unl)
  l <- length(prioris_cut)
  matrix <- matrix(NA, nrow = k, ncol = l)
  
  for(i in 1:length(prioris_cut)) {
    priori <- prioris_cut[i]
    for(j in 1:nrow(X_unl)) {
      X_new <- rbind(X, X_unl[j,])
      Y_new <- c(Y, y_pseudo[j])
      mod <- train_gnb(X_new, Y_new, priori)
      new <- likelihood(mod, X_new)
      matrix[j,i] <- new
    }
  }
  select <- e_admissible_creterion(matrix)
  print(length(select))

  X <- rbind(X, X_unl[select,])
  Y <- c(Y, y_pseudo[select])
  
  X_unl <- X_unl[-select,]
  y_pseudo <- y_pseudo[-select]

  ######### Accuracy 
  gnb_model <- train_gnb(X, Y, prior_p = max_priori)
  y_pred <- predict_gnb(gnb_model, X_test)
  acc <- mean(y_pred == y_test)
  
  for(o in 1:(length(select))) {
    
    result <- c(result, acc) 
    z = z + 1
  }
  
  print(paste("round:", z))
  print(nrow(X_unl))
  

}




