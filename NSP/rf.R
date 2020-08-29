library(randomForest)
library(caret)

# Read Data
setwd("~/Documents/Data Analytics/DataR/FHR")
data <- read.csv("CTG.csv", header = TRUE)
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123) #Random Seed
#Taking independet sample from data
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest

set.seed(222)
#Model : NSP ~ all the others variables on data
rf <- randomForest(NSP~., data=train)
print(rf)
attributes(rf)

# Prediction & Confusion Matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)

# Error rate of Random Forest
plot(rf)

# Tune mtry
t <- tuneRF(train[,-22], train[,22],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)

#New model
rf1 <- randomForest(NSP~., data=train,
                   ntree = 300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
rf2 <- randomForest(NSP~., data=train,
                    ntree = 300,
                    mtry = 16,
                    importance = TRUE,
                    proximity = TRUE)
print(rf1)
print(rf2)
print(rf)

# Prediction & Confusion Matrix - train data
p1 <- predict(rf1, train)
confusionMatrix(p1, train$NSP)
p2 <- predict(rf1, train)
confusionMatrix(p2, train$NSP)

# No. of nodes for the trees
hist(treesize(rf1),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf1,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf1)
varUsed(rf1)

# Partial Dependence Plot
partialPlot(rf1, train, ASTV, "2")

# Extract Single Tree from the random forest
getTree(rf1, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf1, train$NSP)
