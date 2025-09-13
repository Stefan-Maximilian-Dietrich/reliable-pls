library(caret)
library(nnet)
library(ggplot2)

set.seed(123)

res <- list()
for(i in 1:300) {
  data(iris)
  df <- iris
  
  # Speicher-Container
  results <- data.frame(train_size = numeric(), accuracy = numeric())
  
  # Prozentsätze für Trainingsdaten
  splits <- seq(3, 60, by = 1)  # 10% bis 90%
  
  for (n in splits) {
    # Split
    
    idx <- sample(1:nrow(iris), n)
    
    
    train <- df[idx, ]
    test  <- df[-idx, ]
    print(nrow(train))
    
    # Preprocessing
    pp <- preProcess(train[, 1:4], method = c("center", "scale"))
    train_x <- predict(pp, train[, 1:4])
    test_x  <- predict(pp,  test[, 1:4])
    
    # Modell
    nn <- nnet(train_x, class.ind(train$Species), size = 5, decay = 1e-3, maxit = 300, trace = FALSE, MaxNWts = 5000)
    
    # Vorhersage
    pred_prob<- predict(nn, test_x, type = "raw")
    pred_class <- colnames(pred_prob)[apply(pred_prob, 1, which.max)]
    
    acc <- mean(pred_class == test$Species)
    
    # Ergebnisse abspeichern
    results <- rbind(results,
                     data.frame(train_size = nrow(train),
                                accuracy = acc))
  }
  res[[i]] <- results
}

# Grafik

train <- res[[1]][1]
acc <- lapply(res, function(x) x[2] )
rowMeans(as.data.frame(acc))
results <- cbind(train, MeanAccuracy = rowMeans(as.data.frame(acc)))

  ggplot(results, aes(x = train_size, y = MeanAccuracy)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Trainingsinstanzen",
       y = "Test Accuracy",
       title = "Accuracy vs. Trainingsgröße (Iris + nnet)")
