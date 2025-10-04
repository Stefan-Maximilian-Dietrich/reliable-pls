name_dat <- "Digits"
potential <- ave_accuracy_matrix(labled = 10:15, hidden = 1:5, dat = name_dat, N = 4, workers = 4, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Breast_Cancer"
potential <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Cassini"
potential <- ave_accuracy_matrix(labled = 3:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Circle"
potential <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Ionosphere"
potential <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Iris"
potential <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Penguines"
potential <- ave_accuracy_matrix(labled = 3:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Seeds"
potential <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Simplex"
potential <- ave_accuracy_matrix(labled = 6:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Simulates_A"
potential <- ave_accuracy_matrix(labled = 5:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Waveform"
potential <- ave_accuracy_matrix(labled = 3:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))

name_dat <- "Wine"
potential <- ave_accuracy_matrix(labled = 3:100, hidden = 1:25, dat = name_dat, N = 100, workers = 20, metric = "Accuracy")
save(potential, file = paste0("/dss/dsshome1/03/di35lox/MASTER/results/NeuronalNet_potential/", name_dat))





load("/Users/Stefan/Desktop/Banknote")
load("/Users/Stefan/Desktop/Breast_Cancer")
load("/Users/Stefan/Desktop/NeuronalNet_potential/Diabetes")

X <- potential
mean_mat <- Reduce("+", X) / length(X)
var_mat <- Reduce("+", lapply(X, function(m) (m - mean_mat)^2)) / length(X)
sd_mat <- sqrt(var_mat)

df <- melt(mean_mat)

# Heatmap mit ggplot2
ggplot(df, aes(x = hidden, y = labled, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +       # schöne Farbskala
  scale_y_reverse() +            # damit "oben" auch oben ist
  labs(x = "hidden", y = "labled", fill = "Accuracy") +
  theme_minimal()


df <- melt(sd_mat)
# Heatmap mit ggplot2
ggplot(df, aes(x = hidden, y = labled, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +       # schöne Farbskala
  scale_y_reverse() +            # damit "oben" auch oben ist
  labs(x = "hidden", y = "labled", fill = "SD") +
  theme_minimal()


plot_ly(
  x = colnames(mean_mat),
  y = rownames(mean_mat),
  z = mean_mat,
  type = "surface"
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Hidden",  range = c(0, max(as.numeric(colnames(mean_mat))))),
      yaxis = list(title = "Labeled", range = c(0, max(as.numeric(rownames(mean_mat))))),
      zaxis = list(title = "Accuracy",range = c(0, max(mean_mat)))
    )
  )



install.packages("plotly")
library(plotly)