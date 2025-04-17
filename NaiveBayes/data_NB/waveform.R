wave <- mlbench.waveform(n = 5000) %>% as.data.frame()
data <- wave[, c(22,1:21)]
names(data)[1] <- "target"
formula  <- as.formula(paste("target ~", paste(paste0("x.", 1:21), collapse = " + ")))  # Formel als String
