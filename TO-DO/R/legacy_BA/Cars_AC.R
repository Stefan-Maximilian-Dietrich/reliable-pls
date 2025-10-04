library(dplyr)
N = 100
share_unlabeled = 0.8
n = nrow(mtcars)
p = 3
# read in data frame
data_frame = mtcars
name_df = "mtcars" # for results 
data = "mtcars"

target = "vs" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)
data_frame$vs <- as.numeric(data_frame$vs) - 1




#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


test_rows = sample(nrow(data_frame), size = n_test)
test_data = data_frame[test_rows,]

# data frame for SSL
data = data_frame[-test_rows,]

# share of unlabeled obs
n_imp = (nrow(data) * share_unlabeled) %>% round()

# create data setup by randomly unlabelling data points
unlabeled_data_inst <- sample(nrow(data), n_imp)
labeled_data <- data[-unlabeled_data_inst,]
labeled_data <- cbind(labeled_data,nr = 0)
unlabeled_data <- data[unlabeled_data_inst,]

# formula for glm
formula = vs ~1 + mpg + cyl + disp #+ hp + drat +wt + qsec + am + gear + carb

logistic_model <- glm(formula = formula, 
                      data = data_frame, 
                      family = "binomial")
logistic_model %>% summary()


