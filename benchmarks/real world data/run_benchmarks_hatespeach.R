set.seed(2037420)
library(dplyr)
library(MixGHD)
N = 100
share_unlabeled = 0.8
p = 4
n = 120

Daten <- read.csv("data/hate_spreach.csv")
Daten <- as.data.frame(Daten)

Daten$X <- as.numeric(Daten$X)
Daten$id <- as.numeric(Daten$id)
Daten$age <- as.numeric(Daten$age)
Daten$gender <- as.numeric(Daten$gender)
Daten$afam <- as.factor(Daten$afam)
Daten$asian <- as.factor(Daten$asian)
Daten$hispanic <- as.factor(Daten$hispanic)
Daten$white <- as.factor(Daten$white)
Daten$race_other <- as.factor(Daten$race_other)
Daten$race_not_say <- as.factor(Daten$race_not_say)
Daten$education <- as.numeric(Daten$education)
Daten$sexuality <- as.factor(Daten$sexuality)
Daten$english <- as.factor(Daten$english)
Daten$tw_use <- as.factor(Daten$tw_use)
Daten$social_media_use <- as.factor(Daten$social_media_use)
Daten$prolific_hours <- as.numeric(Daten$prolific_hours)
########################## Task
Daten$task_fun <- as.numeric(Daten$task_fun)
Daten$task_interesting <- as.factor(Daten$task_interesting)
Daten$task_boring <- as.factor(Daten$task_boring)
Daten$task_repetitive <- as.factor(Daten$task_repetitive)
Daten$task_important <- as.factor(Daten$task_important)
Daten$task_depressing <- as.factor(Daten$task_depressing)
Daten$task_offensive <- as.factor(Daten$task_offensive)
Daten$another_tweettask <- as.factor(Daten$another_tweettask)
Daten$another_hatetask <- as.factor(Daten$another_hatetask)
######################### Paradaten 
Daten$page_history <- as.character(Daten$page_history)
Daten$date_of_first_access <- as.numeric(Daten$date_of_first_access)
Daten$date_of_last_access <- as.numeric(Daten$date_of_last_access)
Daten$duration_sec <- as.numeric(Daten$duration_sec)
########################## Version
Daten$version <- as.factor(Daten$version)
########################### LABLES
Daten$tw1 <- as.factor(Daten$tw1)
Daten$tw2 <- as.factor(Daten$tw2)
Daten$tw3 <- as.factor(Daten$tw3)
Daten$tw4 <- as.factor(Daten$tw4)
Daten$tw5 <- as.factor(Daten$tw5)
Daten$tw6 <- as.factor(Daten$tw6)
Daten$tw7 <- as.factor(Daten$tw7)
Daten$tw8 <- as.factor(Daten$tw8)
Daten$tw9 <- as.factor(Daten$tw9)
Daten$tw10 <- as.factor(Daten$tw10)
Daten$tw11 <- as.factor(Daten$tw11)
Daten$tw12 <- as.factor(Daten$tw12)
Daten$tw13 <- as.factor(Daten$tw13)
Daten$tw14 <- as.factor(Daten$tw14)
Daten$tw15 <- as.factor(Daten$tw15)
Daten$tw16 <- as.factor(Daten$tw16)
Daten$tw17 <- as.factor(Daten$tw17)
Daten$tw18 <- as.factor(Daten$tw18)
Daten$tw19 <- as.factor(Daten$tw19)
Daten$tw20 <- as.factor(Daten$tw20)
############################## Paradaten
Daten$tw_duration_1 <- as.numeric(Daten$tw_duration_1)
Daten$tw_duration_2 <- as.numeric(Daten$tw_duration_2)
Daten$tw_duration_3 <- as.numeric(Daten$tw_duration_3)
Daten$tw_duration_4 <- as.numeric(Daten$tw_duration_4)
Daten$tw_duration_5 <- as.numeric(Daten$tw_duration_5)
Daten$tw_duration_6 <- as.numeric(Daten$tw_duration_6)
Daten$tw_duration_7 <- as.numeric(Daten$tw_duration_7)
Daten$tw_duration_8 <- as.numeric(Daten$tw_duration_8)
Daten$tw_duration_9 <- as.numeric(Daten$tw_duration_9)
Daten$tw_duration_10 <- as.numeric(Daten$tw_duration_10)
Daten$tw_duration_11 <- as.numeric(Daten$tw_duration_11)
Daten$tw_duration_12 <- as.numeric(Daten$tw_duration_12)
Daten$tw_duration_13 <- as.numeric(Daten$tw_duration_13)
Daten$tw_duration_14 <- as.numeric(Daten$tw_duration_14)
Daten$tw_duration_15 <- as.numeric(Daten$tw_duration_15)
Daten$tw_duration_16 <- as.numeric(Daten$tw_duration_16)
Daten$tw_duration_17 <- as.numeric(Daten$tw_duration_17)
Daten$tw_duration_18 <- as.numeric(Daten$tw_duration_18)
Daten$tw_duration_19 <- as.numeric(Daten$tw_duration_19)
Daten$tw_duration_20 <- as.numeric(Daten$tw_duration_20)
# Unternehmesdaten
Daten$num_approvals <- as.numeric(Daten$num_approvals)
Daten$num_rejections <- as.numeric(Daten$num_rejections)
Daten$prolific_score <- as.numeric(Daten$prolific_score)
# personenbezogene Daten
Daten$countryofbirth <- as.character(Daten$countryofbirth)
Daten$currentcountryofresidence <- as.factor(Daten$currentcountryofresidence)
Daten$employmentstatus <- as.numeric(Daten$employmentstatus)
Daten$firstlanguage <- as.character(Daten$firstlanguage)
Daten$nationality <- as.character(Daten$nationality)
Daten$studentstatus <- as.factor(Daten$studentstatus)

Names <- c("X", "id", "age", "gender", "afam", "asian", "hispanic", "white", "race_other",
           "race_not_say", "education", "sexuality", "english","tw_use", "social_media_use",         
           "prolific_hours", "task_fun", "task_interesting", "task_boring", "task_repetitive",
           "task_important", "task_depressing", "task_offensive", "another_tweettask",
           "another_hatetask", "page_history", "date_of_first_access","date_of_last_access",
           "duration_sec", "version", "num_approvals", "num_rejections", "prolific_score",
           "countryofbirth", "currentcountryofresidence", "employmentstatus", "firstlanguage",
           "nationality", "studentstatus") 
Daten_long <- data.frame()
for(i in 1:20) {
  duration <- paste("tw_duration_", i, sep = "")
  lable <-    paste("tw", i, sep = "")
  Duration <- Daten[[duration]]
  Lable <- Daten[[lable]]
  Daten_L <- Daten[,Names]
  Daten_Lo <- cbind(Daten_L, Tweet = i, Lable, Duration)
  Daten_long <- rbind(Daten_long, Daten_Lo)
}

View(Daten_long)

#Zielvariable -> Binominal 
Daten_long$Hate_Speech <- 0
Daten_long$Hate_Speech[Daten_long$Lable == "hate speech"] <- 1

data_frame <- Daten_long
names(data_frame)[names(data_frame) == 'Hate_Speech'] <- 'target'

#get number of instances
data_frame = data_frame[sample(nrow(data_frame), n),]


#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)


name_df = "hate_speech" # for results 
data = "hate_speech"
# formula for glm
formula = target ~  age + gender + education + white #Length + Left + Right + Bottom + Top + Diagonal    
target = "target" 

View(data_frame)
glm(formula = formula, data = data_frame, family = binomial()) %>% summary


formula_alt1 =  target ~1 + age + gender + education 
formula_alt2 =  target ~1 + age + gender 
formula_alt3 =  target ~1 + age 
formula_alt4 =  target ~1 + gender 
formula_alt5 =  target ~1 + education 
formula_alt6 =  target ~1 + white
formula_list = list(formula, formula_alt1, formula_alt2, formula_alt3)#, formula_alt4, formula_alt5, formula_alt6)

mu_priori_lower <- c(-2, -1, -1, -1, -1)
mu_priori_upper <-  c(0, 0, 1, 1, 1) 
sigma_priori <- matrix(c(3,0,0,0,0,
                         0,1,0,0,0,
                         0,0,2,0,0,
                         0,0,0,1,0,
                         0,0,0,0,2), nrow = 5, byrow = TRUE)
alpha = 0.8

source(paste(getwd(),"/R/Alpha_cut/benchmark-alpha-cut.R", sep = ""))
