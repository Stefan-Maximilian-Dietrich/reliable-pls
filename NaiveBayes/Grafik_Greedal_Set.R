library(ggplot2)

# Werte vorbereiten
x_points <- seq(0.05, 0.95, by = 0.025)   # Punkte bei allen 0.05
x_ticks <- seq(0.1, 0.9, by = 0.1)       # Ticks bei allen 0.1
x_labels <- seq(0.2, 0.8, by = 0.2)      # Beschriftung bei allen 0.2

# Datenrahmen
df_points <- data.frame(x = x_points, y = 0)
df_ticks  <- data.frame(x = x_ticks)
df_labels <- data.frame(
  x = x_labels,
  label_top = sprintf("%.1f", rev(x_labels)),
  label_bottom = sprintf("%.1f", x_labels)
)

# Justierung
offset_y <- 0.05  # Noch weniger Abstand der Beschriftungen zum Zahlenstrahl
label_offset_x <- 0.01  # Weniger Versatz nach links

# Plot
p2 <- ggplot() +
  # Zahlenstrahl
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), size = 1) +
  # Punkte
  geom_point(data = df_points, aes(x = x, y = y), size = 2, color = "red") +
  # Ticks
  geom_segment(data = df_ticks, 
               aes(x = x, xend = x, y = -0.025, yend = 0.025),
               linewidth = 0.3) +
  # Beschriftung oben
  geom_text(data = df_labels, aes(x = x, y = offset_y, label = label_top), size = 3) +
  # Beschriftung unten
  geom_text(data = df_labels, aes(x = x, y = -offset_y, label = label_bottom), size = 3) +
  # P(A) und P(B) am linken Rand
  annotate("text", x = 0 - label_offset_x, y = offset_y, 
           label = expression(P(C[1])), hjust = 1, fontface = "bold", size = 3.5) +
  annotate("text", x = 0 - label_offset_x, y = -offset_y, 
           label = expression(P(C[2])), hjust = 1, fontface = "bold", size = 3.5) +
  # Layout
  theme_void() +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.2, 0.2))

# Plot anzeigen
print(p2)
ggsave(paste("/Users/Stefan/Desktop/", "2D", ".pdf", sep = ""), width = 15, height = 5, units = "cm", dpi = 300,plot = p2)

####### 3D

# ----------------------------------------------
# Komplettes R-Skript für ein Ternärdiagramm
# mit Achsen in 0.1-Schritten von 0 bis 1
# ----------------------------------------------
# ------------------------------------------------------
# Sauberes R-Skript für ein Ternärdiagramm mit 0.1-Schritten
# ------------------------------------------------------
# ----------------------------------------------
# Komplettes R-Skript für ein Ternärdiagramm
# ----------------------------------------------

# Paket installieren (nur beim ersten Mal nötig)
if (!require("ggtern")) {
  install.packages("ggtern")
  library(ggtern)
} else {
  library(ggtern)
}

# Beispiel-Daten: Jede Zeile entspricht einem Punkt im Dreieck
df <- data.frame(
  E1 = c(0.2, 0.5, 0.7, 0.3),
  E2 = c(0.3, 0.2, 0.1, 0.4),
  E3 = c(0.5, 0.3, 0.2, 0.3)  # Beachte: E1 + E2 + E3 = 1
)
df <- generate_priori_simplex(c("a", "b", "c"), step = 0.1)
# Zeichne das baryzentrische Koordinatensystem mit Punkten
p3 <- ggtern(data = df, aes(x = a, y = b, z = c)) +
  geom_point(size = 2, color = "red") +
  theme_bw() +
  labs(
    T = expression(P(C[3])),
    L = expression(P(C[1])),
    R = expression(P(C[2]))
  )
ggsave(paste("/Users/Stefan/Desktop/", "3D", ".pdf", sep = ""), width = 15, height = 5, units = "cm", dpi = 300,plot = p3)

