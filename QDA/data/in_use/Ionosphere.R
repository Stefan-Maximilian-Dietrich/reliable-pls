# read in data frame
data(Ionosphere)
data_frame <- Ionosphere %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Class'] <- 'target'
data <- data_frame[,c(35,1:34)]

# formula for glm
vars <- c("target ~")
for (v in 2:35) {
  vars <- c(vars, colnames(data)[v])
}
formula = paste(vars, collapse=" + ") %>% as.formula()

