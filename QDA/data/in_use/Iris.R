data <- iris[,c(5,1,2,3,4)]
names <- names(data)
new_names <- c("target", names[-1])
names(data) <- new_names

formula = target ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
data_name <- "iris"

