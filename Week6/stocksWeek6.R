setwd('C:/Home/Courses/The Analytics Edge/Week6')
Sys.setlocale('LC_ALL', 'C')

stocks = read.csv('../data/StocksCluster.csv')

# 1.1
str(stocks)

# 1.2
mean(stocks$PositiveDec)

# 1.3
sort(cor(stocks))

# 1.4
sort(colMeans(stocks))

# 2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio=0.7)
stocksTrain = subset(stocks, spl==T)
stocksTest = subset(stocks, spl==F)
LR = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
table(stocksTrain$PositiveDec, predict(LR, type='response') > 0.5)
(990+3640) / nrow(stocksTrain)

# 2.2
table(stocksTest$PositiveDec, predict(LR, newdata=stocksTest, type='response') > 0.5)
(417+1553) / nrow(stocksTest)

# 2.3
mean(stocksTest$PositiveDec)

# 3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

# 3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

# 3.4
set.seed(144)
clusterGroups = kmeans(normTrain, centers=3)
table(clusterGroups$cluster)

# 3.5
library(flexclust)
km.kcca = as.kcca(clusterGroups, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

# 4.1
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# 4.2
LR1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
LR2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
LR3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
summary(LR1)
summary(LR2)
summary(LR3)

# 4.3
table(stocksTest1$PositiveDec, predict(LR1, newdata=stocksTest1, type='response') > 0.5)
(30+774) / nrow(stocksTest1)
table(stocksTest2$PositiveDec, predict(LR2, newdata=stocksTest2, type='response') > 0.5)
(388+757) / nrow(stocksTest2)
table(stocksTest3$PositiveDec, predict(LR3, newdata=stocksTest3, type='response') > 0.5)
(49+13) / nrow(stocksTest3)

# 4.4
PredictTest1 = predict(LR1, newdata=stocksTest1, type='response')
PredictTest2 = predict(LR2, newdata=stocksTest2, type='response')
PredictTest3 = predict(LR3, newdata=stocksTest3, type='response')
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544) / (467+1544+353+1110)
