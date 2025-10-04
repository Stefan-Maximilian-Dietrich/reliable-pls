library(RCurl)
UCI_data_URL <- RCurl::getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data')
names <- c('id_number', 'diagnosis', 'radius_mean', 
           'texture_mean', 'perimeter_mean', 'area_mean', 
           'smoothness_mean', 'compactness_mean', 
           'concavity_mean','concave_points_mean', 
           'symmetry_mean', 'fractal_dimension_mean',
           'radius_se', 'texture_se', 'perimeter_se', 
           'area_se', 'smoothness_se', 'compactness_se', 
           'concavity_se', 'concave_points_se', 
           'symmetry_se', 'fractal_dimension_se', 
           'radius_worst', 'texture_worst', 
           'perimeter_worst', 'area_worst', 
           'smoothness_worst', 'compactness_worst', 
           'concavity_worst', 'concave_points_worst', 
           'symmetry_worst', 'fractal_dimension_worst')
breast_cancer <- read.table(textConnection(UCI_data_URL), sep = ',', col.names = names)

breast_cancer$id_number <- NULL


data_frame <- breast_cancer %>% as.data.frame()
names(data_frame)[names(data_frame) == 'diagnosis'] <- 'target'

#MCAR
data_frame = na.omit(data_frame)

p = 30

# formula for glm
vars <- c("target ~")
for (v in 2:p) {
  vars <- c(vars, colnames(data_frame)[v])
}
#vars = c(vars, "s(radius_mean)")

formula = paste(vars, collapse=" + ") %>% as.formula()
formula

target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
data <- data_frame 
data <- data[, all.vars(formula)]