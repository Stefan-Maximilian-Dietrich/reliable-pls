Predictions <- as.data.frame(predict(logistic_model_100,  df[101:150, ], type = "response"))
names(Predictions) <- "prediction"
Predictions$pseudos <- as.numeric(Predictions$prediction > 0.5)
Predictions$lable <- df[101:150, c(1)]

Predictions <- as.data.frame(predict(logistic_model,  df[101:150, ], type = "response"))
names(Predictions) <- "prediction"
dfp <- df[101:150, ]
dfp$y <- as.numeric(Predictions$prediction > 0.5)
dfp <- rbind(df[1:100, ],dfp )

logistic_model_100 <- glm(formula = formula, data = data100, family = "binomial")
summary(logistic_model_100)

logistic_model_300 <- glm(formula = formula, data = df[1:300, ], family = "binomial")
summary(logistic_model_150)

logistic_model_100_50 <- glm(formula = formula, data = dfp, family = "binomial")
summary(logistic_model_100_50)

df1 <- rbind(df[0:117, ], df[119:4000, ])
logistic_model_138 <- glm(formula = formula, data = df1, family = "binomial")
Predictions149 <- as.data.frame(predict(logistic_model_138,  df[118, ], type = "response"))
Predictions100 <- as.data.frame(predict(logistic_model_100,  df[118, ], type = "response"))

# Verbesserung der suche 

logistic_model_50 <- glm(formula = formula, data = df[1:50,], family = "binomial")
summary(logistic_model_50)
logistic_model_5 <- glm(formula = formula, data = df[1:5,], family = "binomial")
summary(logistic_model_5)
logistic_model_15 <- glm(formula = formula, data = df[1:15,], family = "binomial")
summary(logistic_model_5)
 #Test 
Test_acc(data1000_t, logistic_model_5)
Test_acc(data1000_t, logistic_model_15)
Test_acc_diff(data5000_t, df[8:22,], df[5:6,])



