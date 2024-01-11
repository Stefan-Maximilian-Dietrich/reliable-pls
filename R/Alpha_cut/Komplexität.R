Zeit_1 <- 1.2
Zeit_2 <- 1.5
Zeit_3 <- 1.9

m_t <- 200
m <- (1:m_t)

Lable_1 <- rep(c("1.2s seriell"), times = m_t)
Lable_2 <- rep(c("1.5s seriell"), times = m_t)
Lable_3 <- rep(c("1.9s seriell"), times = m_t)
Lable_4 <- rep(c("1.2s parallel"), times = m_t)
Lable_5 <- rep(c("1.5s parallel"), times = m_t)
Lable_6 <- rep(c("1.9s parallel"), times = m_t)


Auswührungen <- (m^2 + m )/2
Auswührungen_p <- ((ceiling(m/10))^2 + (ceiling(m/10)))/2

Zeit <- Auswührungen * Zeit

Zeit <- c(Auswührungen * Zeit_1, Auswührungen * Zeit_2, Auswührungen * Zeit_3, Auswührungen_p * Zeit_1, Auswührungen_p * Zeit_2, Auswührungen_p * Zeit_3)
Labels <- c(Lable_1, Lable_2,Lable_3,Lable_4,Lable_5,Lable_6)
Größe <- c(m,m,m,m,m,m)

Resulta <- data.frame(Größe, Labels, Zeit)

View(Resulta)

ggplot(data = Resulta, aes(x = Größe, y = Zeit, group =Labels )) +
  geom_line(color = Labels)

Zeit2 <- c(Auswührungen_p * Zeit_1, Auswührungen_p * Zeit_2, Auswührungen_p * Zeit_3)
Resulta2 <- data.frame(Größe, Labels, Zeit2)

ggplot(data = Resulta2, aes(x = Größe, y = Zeit2, group =Labels )) +
  geom_line(color = Labels)


