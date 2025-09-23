data(Sonar)
data_S <- Sonar
data <- data_S[,c(61, 1:60)]
names(data)[1] <- "target"
formula  <- as.formula(paste("target ~", paste(paste0("V", 1:60), collapse = " + ")))  # Formel als String



