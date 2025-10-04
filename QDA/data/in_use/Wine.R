task = tsk("wine")
data = task$data()
names(data)[1] <- "target"
target <- data$target
A <- sapply(data, function(x) length(unique(x))) 
nrow(data)
data <- as.data.frame(data)

vars <- c("target ~")
for (v in 2:13) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()
data <- data[, all.vars(formula)]