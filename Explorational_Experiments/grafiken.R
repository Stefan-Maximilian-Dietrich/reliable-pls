install.packages("ggtern")

library(ggtern)

# Schrittweite festlegen: je kleiner der Schritt, desto feiner das Netz
step <- 0.05

# Erzeuge ein Gitter für die Dimensionen A und B
grid <- expand.grid(A = seq(0, 1, by = step),
                    B = seq(0, 1, by = step))
# Behalte nur die Punkte, bei denen A + B <= 1 gilt (da C = 1 - A - B >= 0 sein muss)
grid <- subset(grid, A + B <= 1)
# Berechne den Wert für C, sodass A + B + C = 1 erfüllt ist
grid$C <- 1.00 - (grid$A + grid$B)

# Zeichne das Ternärdiagramm mit explizit auf 0 bis 1 gesetzten Achsen
ggtern(data = grid, aes(x = A, y = B, z = C)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Ternäres Diagramm mit Punkten (Skala 0 bis 1)",
       T = "A", L = "B", R = "C") +
  theme_bw()




generate_priori_simplex <- function(dimensions, step = 0.1) {
  # Erzeuge für jede Dimension eine Sequenz von 0 bis 1 mit der gegebenen Schrittweite
  seq_list <- replicate(dimensions, seq(0, 1, by = step), simplify = FALSE)
  
  # Erzeuge das vollständige Gitter
  grid <- do.call(expand.grid, seq_list)
  

  # Behalte nur die Zeilen, bei denen die Summe der Koordinaten ca. 1 ergibt
  simplex <- grid[rowSums(grid) == 1.000, ]
  
  # Optional: Setze die Spaltennamen, z. B. X1, X2, ..., Xd
  colnames(simplex) <- paste0("X", 1:dimensions)
  
  return(simplex)
}

# Beispiel: Simplex in 3 Dimensionen mit Schrittweite 0.1
simplex3d <- generate_simplex(3, step = 0.1)
head(simplex3d)

