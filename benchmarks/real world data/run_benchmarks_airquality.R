data(airquality)
set.seed(2037420)

library(dplyr)
library(MixGHD)
data_frame <- airquality[!is.na(airquality$Ozone),]
data_frame$Solar.R <- as.numeric(data_frame$Solar.R)
data_frame$Wind <- as.numeric(data_frame$Wind)
data_frame$Temp <-as.numeric(data_frame$Temp)
data_frame$target <- as.numeric(data_frame$Ozone > 70) #wenn der Grenzwert von über 70 überschritten wird

N = 100
share_unlabeled = 0.8
p = 3
n = nrow(data)

n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "airquality" # for results 
data = "airquality"

formula = target ~  Solar.R + Wind + Temp #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 

formula_alt1 =  target ~ 1 + Solar.R + Wind 
formula_alt2 =  target ~ 1 +  Wind + Temp
formula_alt3 =  target ~ 1 + Solar.R +  Temp
formula_alt4 =  target ~ 1 + Solar.R
formula_alt5 =  target ~ 1 + Wind 
formula_alt6 =  target ~ 1 + Temp

formula_list = list(formula, formula_alt1, formula_alt2, formula_alt3)

###################
mu_priori_lower <- c(-2, -2, -2, -2)
mu_priori_upper <-  c(2, 2, 2, 2)
sigma_priori <- diag(1, nrow = 4)
alpha = 0.8
###################

path_to_experiments = paste(getwd(),"/benchmarks/experiments", sep = "")
# sequential sourcing
# miceadds::source.all(path_to_experiments) 


# parallel sourcing
source(paste(getwd(),"/R/Alpha_cut/benchmark-alpha-cut.R", sep = ""))

files_to_source = list.files(path_to_experiments, pattern="*.R",
                             full.names = TRUE)

num_cores <- parallel::detectCores() - 1
comp_clusters <- parallel::makeCluster(num_cores) # parallelize experiments
doParallel::registerDoParallel(comp_clusters)
object_vec = c("N", "share_unlabeled", "data_frame", "name_df", "formula", "formula_list", "target", "p", "n_test")
env <- environment()
parallel::clusterExport(cl=comp_clusters, varlist = object_vec, envir = env)
parallel::parSapply(comp_clusters, files_to_source, source)

parallel::stopCluster(comp_clusters)






