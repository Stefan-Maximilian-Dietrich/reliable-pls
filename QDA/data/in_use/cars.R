data_frame = mtcars

target = "vs" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
data = data_frame
data <- data[, c(8,1:7,9:11)]
names <- names(data)
new_names <- c("target", names[-1])
names(data) <- new_names

formula = target ~  mpg + cyl + disp #+ hp + drat +wt + qsec + am + gear + carb

vars <- all.vars(formula)

# Dataframe auf diese Spalten reduzieren:
data <- data[, vars]