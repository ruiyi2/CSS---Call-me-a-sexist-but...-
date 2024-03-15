#### second nrc bind (full sentiment) ####
source(file = "3_Prediction_Modelling.R")

# Set seed
set.seed(123)

# Split data into train and test data
train_index1 <- sample(1:nrow(complete), 0.8 * nrow(complete))
train_data1 <- complete[train_index1, ]
test_data1 <- complete[-train_index1, ]

summary(nrc_vector)

complete$dataset <- as.factor(complete$dataset)

# Build prediction model with logistic regression (train test)
model1 <- glm(sexist ~ dataset + toxicity + anger + negative + positive + disgust + fear + sadness +
                anticipation + joy + surprise + trust, 
              family = binomial(link = "logit"), 
              data = train_data1)

summary(model1)


# Make predictions on test data
predictions1 <- predict(model1, newdata = test_data1, type = "response")

pred1 <- ifelse(predictions1 < 0.5, 0, 1)

# binding original test data with the predictions
testsexist1 <- cbind(test_data1$sexist, pred1)
testsexist1 <- as.data.frame(testsexist1)
testsexist1$pred1 <- as.factor(testsexist1$pred1)


# Calculate evaluation metrics for the test data
accuracy_test1 <- mean(pred1 == test_data1$sexist)
rmse_test1 <- sqrt(mean((pred1 - test_data1$sexist)^2))

# Visualize the comparison for the entire test dataset
comparison_df_test1 <- data.frame(Actual = testsexist1$V1, Predicted = testsexist1$pred1)

table(comparison_df_test1$Predicted) # 2530 FALSE, 197 TRUE
table(comparison_df_test1$Actual) # 2370 FALSE, 357 TRUE

ggplot(comparison_df_test1, aes(x = Actual)) +
  geom_bar()
ggplot(comparison_df_test1, aes(x = Predicted)) +
  geom_bar()
# left: actual, top predicted:
table(comparison_df_test1$Actual, comparison_df_test1$Predicted)


#### Build prediction model with logistic regression (full dataset) ####
model2 <- glm(sexist ~ dataset + toxicity + anger + negative + positive + disgust + fear + sadness +
                anticipation + joy + surprise + trust, 
              family = binomial(link = "logit"), 
              data = complete)

summary(model2)

# Make predictions on full data
predictions2 <- predict(model2, newdata = complete, type = "response")

pred2 <- ifelse(predictions2 < 0.5, 0, 1)

# binding original test data with the predictions
testsexist2 <- cbind(complete$sexist, pred2)
testsexist2 <- as.data.frame(testsexist2)

# Calculate evaluation metrics for the test data
accuracy_test2 <- mean(pred2 == complete$sexist)
rmse_test2 <- sqrt(mean((pred2 - complete$sexist)^2))

# Visualize the comparison for the entire test dataset
comparison_df_test2 <- data.frame(Actual = testsexist2$V1, Predicted = testsexist2$pred2)
comparison_df_test2$Predicted <- as.factor(comparison_df_test2$Predicted)
table(comparison_df_test2$Predicted) # 12668 FALSE, 963 TRUE
table(comparison_df_test2$Actual) # 11822 FALSE, 1809 TRUE


ggplot(comparison_df_test2, aes(x = Actual)) +
  geom_bar()
ggplot(comparison_df_test2, aes(x = Predicted)) +
  geom_bar()
# left: actual, top predicted:
table(comparison_df_test2$Actual, comparison_df_test2$Predicted)

# Add prediction of pred2 (full data set and full sentiment score) to the original dataset with full
# sentiment score and compare regarding the datasets
predcomplete2 <- cbind(complete, pred2)

# Filter
cp2 <- predcomplete2 %>%
  filter(pred2 == 1)

table(cp2$dataset)

# Add prediction of pred1 (test data set and full sentiment score) to the test dataset with full 
# sentiment score and compare regarding the datasets
predcomplete1 <- cbind(test_data1, pred1)

# Filter
cp1 <- predcomplete1 %>%
  filter(pred1 == 1)

table(cp1$dataset)
table(test_data1$dataset)

# Add prediction of predf (full data set and one sentiment score) to the original dataset with one 
# sentiment score and compare regarding the datasets
predcompletef <- cbind(complete, predf)

# Filter
cpf <- predcompletef %>%
  filter(predf == 1)

table(cpf$dataset)

# Add prediction of pred (test data set and one sentiment score) to the test dataset with one 
# sentiment score and compare regarding the datasets
predcomplete <- cbind(test_data, pred)

# Filter
cp <- predcomplete %>%
  filter(pred == 1)

table(cp$dataset)
