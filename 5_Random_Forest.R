#### Random Forest approach ####
source("4_Logistic_Regression.R")

# Set seed
set.seed(123)

##### Full dataset and full sentiment #####
complete$sexist <- as.factor(complete$sexist)
rfc <- randomForest(sexist ~ dataset + toxicity + anger + negative + positive + disgust + fear + 
                      sadness + anticipation + joy + surprise + trust, 
                    data = complete, 
                    proximity = TRUE)

# Prediction & Confusion Matrix - full dataset
prfc <- predict(rfc, complete)
confusionMatrix(prfc, complete$sexist)

table(prfc) # 12711 False, 920 True

# Error rate of Random Forest
plot(rfc)

# Tuning possibility
t1 <- tuneRF(complete[, -5], complete[, 5],
             stepFactor = 0.5,
             plot = TRUE,
             ntreeTry = 150,
             trace = TRUE,
             improve = 0.05)

# No. of nodes for the trees
hist(treesize(rfc),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rfc,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rfc)
# dataset is the most important attribute followed by toxicity after that positive


# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rfc, complete$sexist)


##### Test- & Train - dataset and full sentiment #####
# Split data into train and test data
set.seed(123)

train_indexrf <- sample(1:nrow(complete), 0.8 * nrow(complete))
train_datarf <- complete[train_indexrf, ]
test_datarf <- complete[-train_indexrf, ]


rf <- randomForest(sexist ~ dataset + toxicity + anger + negative + positive + disgust + fear + 
                     sadness + anticipation + joy + surprise + trust, 
                   data = train_datarf, 
                   proximity = TRUE)
print(rf)

# Prediction & Confusion Matrix - train data
prf <- predict(rf, train_datarf)
confusionMatrix(prf, train_datarf$sexist)
table(prf) # 10120 non sexist, 784 sexist

# Prediction & Confusion Matrix - test data
prft <- predict(rf, test_datarf)
confusionMatrix(prft, test_datarf$sexist)
table(prft) # 2575 non sexist, 152 sexist

# Error rate of Random Forest
plot(rf)

# high accuracy -> no need for further tuning

# Tuning possibility
t <- tuneRF(train_datarf[, -5], train_datarf[, 5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf)
# dataset is the most important attribute followed by toxicity


# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train_datarf$sexist)
