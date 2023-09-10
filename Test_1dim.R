View(mtcars)
ratio <- 0.5

mtcars_1 <- mtcars[c(1,8)]
model_1 <- glm(formula = vs ~ mpg, data = mtcars_1, family = "binomial")
summary(model_1)

formula <- vs ~ mpg
labeled_data <- mtcars[c(1,8)]

# fit model to labeled data
logistic_model <- glm(formula = formula, 
                      data = labeled_data, 
                      family = "binomial")

# predict on unlabeled data
predicted_target <- predict(logistic_model, 
                            newdata= unlabeled_data, 
                            type = "response")

# assign predicted (pseudo) labels to unlabeled data
unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0) 