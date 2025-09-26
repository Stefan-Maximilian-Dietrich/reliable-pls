A <- ave_accuracy_matrix(labled = 2:4, hidden = 1:5, dat = "Banknote", N = 4, workers = 4, metric = "Accuracy")
B <- ave_accuracy_matrix(labled = 2:40, hidden = 1:20, dat = "Breast_Cancer", N = 10, workers = 4, metric = "Accuracy")
C <- ave_accuracy_matrix(labled = 3:30, hidden = 1:10, dat = "Iris", N = 24, workers = 4, metric = "Accuracy")
D <- ave_accuracy_matrix(labled = 2:100, hidden = 1:25, dat = "Circle", N = 50, workers = 4, metric = "Accuracy")

X <- A
mean_mat <- Reduce("+", X) / length(X)
df <- melt(mean_mat)

# Heatmap mit ggplot2
library(ggplot2)
ggplot(df, aes(x = hidden, y = labled, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +       # schöne Farbskala
  scale_y_reverse() +            # damit "oben" auch oben ist
  labs(x = "hidden", y = "labled", fill = "Accuracy") +
  theme_minimal()
 
