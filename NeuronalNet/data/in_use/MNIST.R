# Falls nicht installiert:
install.packages("keras")
library(keras)

# MNIST laden
mnist <- dataset_mnist()

# Training- und Testdaten
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test  <- mnist$test$x
y_test  <- mnist$test$y

# Dimensionen prüfen
dim(x_train)  # 60000 x 28 x 28
dim(x_test)   # 10000 x 28 x 28
table(y_train)  # Klassenverteilung
