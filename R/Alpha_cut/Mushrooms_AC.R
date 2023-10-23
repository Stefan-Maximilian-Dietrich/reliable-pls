library(dplyr)
library(MixGHD)
N = 40
share_unlabeled = 0.8
p = 3
#n = 500

# read in data frame

data_dir = paste(getwd(),"/data", sep="")
mushrooms= read.csv(paste(data_dir, "/secondary_data.csv",sep=""), sep = ";", na.strings = c('NA'), stringsAsFactors = FALSE)
data_frame <- mushrooms %>% as.data.frame()

data_frame = data_frame %>% dplyr::select(cap.diameter, stem.height, stem.width, class)

names(data_frame)[names(data_frame) == 'class'] <- 'target'
data_frame = na.omit(data_frame)

#get number of instances

data_frame = data_frame[sample(nrow(data_frame), 160),]

#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "mushrooms" # for results 
data = "mushrooms"


formula = target ~ 1 + cap.diameter + stem.height + stem.width


target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$target <- as.numeric(data_frame$target) - 1


