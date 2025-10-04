task = tsk("penguins_simple")
data = task$data()
names(data)[1] <- "target"
target <- data$target

data <- as.data.frame(data)
data <- na.omit(data)[c(1:5)]

vars <- c("target ~")
for (v in 2:5) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()
data <- data[, all.vars(formula)]