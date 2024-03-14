#### Prediction and Modelling ####
source(file = "2_Sentiment_Analysis.R")

# Set seed
set.seed(123)

# Split data into train and test data
train_index <- sample(1:nrow(nrcs), 0.8 * nrow(nrcs))
train_data <- nrcs[train_index, ]
test_data <- nrcs[-train_index, ]

nrcs$dataset <- as.factor(nrcs$dataset)

##### Build prediction model with logistic regression based on only on finite score #####
model <- glm(sexist ~ dataset + toxicity + nrc_vector, family = binomial(link = "logit"), 
             data = train_data)

model_sen <- glm(y ~ anger, family = binomial(link = "logit"), data = complete_sen)
summary(model)
##### Make predictions on test data #####
predictions <- predict(model, newdata = test_data, type = "response")

pred <- ifelse(predictions < 0.5, 0, 1)

# binding original test data with the predictions
testsexist <- cbind(test_data$sexist, pred)
testsexist <- as.data.frame(testsexist)
testsexist$pred <- as.factor(testsexist$pred)


##### Calculate evaluation metrics for the test data #####
accuracy_test <- mean(pred == test_data$sexist)
rmse_test <- sqrt(mean((pred - test_data$sexist)^2))

# Visualize the comparison for the entire test dataset
comparison_df_test <- data.frame(Actual = testsexist$V1, Predicted = testsexist$pred)

# Count of Sexist
table(comparison_df_test$Actual) # 2370 FALSE, 357 TRUE
table(comparison_df_test$Predicted) # 2527 FALSE, 200 TRUE

##### Visualization of the Sexist count #####
ggplot(comparison_df_test, aes(x = Actual)) +
  geom_bar()
ggplot(comparison_df_test, aes(x = Predicted)) +
  geom_bar()
# left: actual, top predicted:
table(comparison_df_test$Actual, comparison_df_test$Predicted)


#### first nrc bind (full data set) ####
# Build prediction model with logistic regression based on only on finite score
modelf <- glm(sexist ~ dataset + toxicity + nrc_vector, family = binomial(link = "logit"), 
              data = nrcs)

summary(modelf)
# Make predictions on test data
predictionsf <- predict(modelf, newdata = nrcs, type = "response")

predf <- ifelse(predictionsf < 0.5, 0, 1)

# binding original test data with the predictions
testsexistf <- cbind(nrcs$sexist, predf)
testsexistf <- as.data.frame(testsexistf)
testsexistf$predf <- as.factor(testsexistf$predf)


# Calculate evaluation metrics for the test data
accuracy_testf <- mean(predf == nrcs$sexist)
rmse_testf <- sqrt(mean((predf - nrcs$sexist)^2))

# Visualize the comparison for the entire test dataset
comparison_df_testf <- data.frame(Actual = testsexistf$V1, Predicted = testsexistf$predf)

# Count of Sexist
table(comparison_df_testf$Actual) # 11822 FALSE, 1809 TRUE
table(comparison_df_testf$Predicted) # 12641 FALSE, 990 TRUE

# Visualization of the Sexist count
ggplot(comparison_df_testf, aes(x = Actual)) +
  geom_bar()
ggplot(comparison_df_testf, aes(x = Predicted)) +
  geom_bar()
# left: actual, top predicted:
table(comparison_df_testf$Actual, comparison_df_testf$Predicted)
