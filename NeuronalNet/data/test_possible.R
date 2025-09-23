A <- ave_accuracy_matrix(labled = 2:4, hidden = 1:5, dat = "Banknote", N = 4, workers = 4, metric = "Accuracy")
C <- ave_accuracy_matrix(labled = 3:6, hidden = 1:5, dat = "Iris", N = 8, workers = 4, metric = "Accuracy")


library(reshape2)
df <- melt(A)

# Heatmap mit ggplot2
library(ggplot2)
ggplot(df, aes(x = hidden, y = labled, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +       # schöne Farbskala
  scale_y_reverse() +            # damit "oben" auch oben ist
  labs(x = "hidden", y = "labled", fill = "Accuracy") +
  theme_minimal()

