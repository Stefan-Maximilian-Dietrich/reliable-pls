d <- 5
tg = mlr3::mlr_task_generators$get("simplex")
tg$param_set$values$d = d

set.seed(1)
task = tg$generate(100000)

# 3. Daten extrahieren
data = task$data()

names(data)[1] <- "target"
target <- data$target

data <- as.data.frame(data)
data <- na.omit(data)

vars <- c("target ~")
for (v in 2:(d+1)) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()