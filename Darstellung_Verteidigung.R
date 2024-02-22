library("ggplot2")
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

name_df = "banknote" # for results 
data = "banknote"
formula = target ~  Diagonal+ Bottom + Length  
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$target <- as.numeric(data_frame$target) - 1 

ggplot(data = data_frame, aes(x = Diagonal, y = Bottom, colour = as.factor(target))) +
  geom_point() +
  theme_minimal() +
  guides(colour = FALSE)

  

plot_ly(x=temp, y=pressure, z=dtime, type="scatter3d", mode="markers", color=temp)


install.packages("rgl")
library(rgl)
library(rgl)
knitr::knit_hooks$set(webgl = rgl::hook_webgl)
options(rgl.useNULL = TRUE)
clear3d()
surface3d(x = 10 * seq_len(nrow(volcano)),
          y = 10 * seq_len(ncol(volcano)),
          z = 2 * volcano, color = "red")

library(rgl)
clear3d()
with(quakes, points3d(long, lat, -depth / 50, size = 2,
                      col = rep("black", nrow(quakes))))


install.packages("plotly")
library(plotly)

# Generate some example data
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  z = rnorm(100)
)

# Create a 3D scatter plot
plot_ly(data, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers") %>%
  layout(scene = list(aspectmode = "cube"))