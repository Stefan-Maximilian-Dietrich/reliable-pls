
categories <- 1:5
varibales <- 60
data  <- NULL
n <- 1000
for(i in categories) {
  for(j in 1:n){
    X <-rnorm(varibales, categories[i], 2)
    point <- c(i, X)
    data <- rbind(data, point)
  }
}

colnames(data) <- c("target", paste0("Var", 1:varibales))
formula  <- as.formula(paste("target ~", paste(paste0("Var", 1:varibales), collapse = " + ")))  # Formel als String
data <- as.data.frame(data)
data$target <- as.factor(data$target)