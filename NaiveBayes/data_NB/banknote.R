library("MixGHD")
data(banknote)
data_frame <- banknote %>% as.data.frame()
names(data_frame)[names(data_frame) == 'Status'] <- 'target'

# formula 
formula = target ~  Diagonal+ Bottom + Length #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()

####### Return
data = data_frame
formula = target ~ Diagonal + Bottom + Length
data_name <- "bank"
#######