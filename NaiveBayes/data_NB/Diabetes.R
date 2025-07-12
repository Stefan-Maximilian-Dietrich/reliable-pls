task = tsk("pima")
data = task$data()
names(data)[1] <- "target"
target <- data$target
data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:8) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

