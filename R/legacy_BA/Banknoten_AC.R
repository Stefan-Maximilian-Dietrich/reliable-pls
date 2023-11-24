N = 100
share_unlabeled = 0.8
mu_priori_lower <- c(-2, -2, -2, -2)
mu_priori_upper <-  c(2, 2, 2, 2)
sigma_priori <- matrix(c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),  nrow = 4)
alpha = 0.8

# read in data frame
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

#get number of instances
data_frame = data_frame[sample(nrow(data_frame), 120),]


#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "banknote" # for results 
data = "banknote"
# formula for glm
formula = target ~  Diagonal+ Bottom + Length #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$target <- as.numeric(data_frame$target) - 1 # Musste uch hinzufügen 




