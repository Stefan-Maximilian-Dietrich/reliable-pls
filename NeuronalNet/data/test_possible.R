A <- ave_accuracy_matrix(labled = 2:50, hidden = 1:10, dat = "Banknote", N = 16, workers = 4, metric = "Accuracy")
B <- ave_accuracy_matrix(labled = 2:50, hidden = 1:10, dat = "Banknote", N = 1, workers = 4, metric = "Accuracy")
C <- ave_accuracy_matrix(labled = 2:50, hidden = 1:10, dat = "Breast_Cancer", N = 8, workers = 4, metric = "Accuracy")


library(reshape2)
df <- melt(B)

# Heatmap mit ggplot2
library(ggplot2)
ggplot(df, aes(x = hidden, y = labled, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +       # schöne Farbskala
  scale_y_reverse() +            # damit "oben" auch oben ist
  labs(x = "Spalten", y = "Zeilen", fill = "Wert") +
  theme_minimal()