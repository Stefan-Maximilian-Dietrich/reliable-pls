# generate two-spirals dataset
spirals <- mlbench.spirals(n = 500, cycles = 1, sd = 0.02)

# put into data frame
spirals_df <- data.frame( target = spirals$classes, spirals$x)
data <- spirals_df

formula = target ~ X1 + X2