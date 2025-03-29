library(MixGHD)
data_frame <- airquality[!is.na(airquality$Ozone),]
data_frame$Solar.R <- as.numeric(data_frame$Solar.R)
data_frame$Wind <- as.numeric(data_frame$Wind)
data_frame$Temp <-as.numeric(data_frame$Temp)
data_frame$target <- as.numeric(data_frame$Ozone > 70) #wenn der Grenzwert von über 70 überschritten wird
data <- data_frame[,c(7,1:6)]
data <- na.omit(data)
data$target <- as.factor(data$target)


formula = target ~  Solar.R + Wind + Temp

