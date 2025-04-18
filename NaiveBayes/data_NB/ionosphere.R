library(dplyr)
library(fdm2id)

# read in data frame
data(ionosphere)
data_frame <- ionosphere %>% as.data.frame()
names(data_frame)[names(data_frame) == 'V35'] <- 'target'
data <- data_frame[,c(34,2:33)]

# formula for glm
vars <- c("target ~")
for (v in 1:33) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

