setwd('C:/Home/Courses/The Analytics Edge/Week4')
Sys.setlocale('LC_ALL', 'C')

census = read.csv('../data/census.csv')
str(census)

# 1.1
library(caTools)
set.seed(2000)
split = sample.split(census, SplitRatio=0.6)
train = subset(census, split==T)
test = subset(census, split==F)

LR = glm(over50k ~ ., data=train, family=binomial)
summary(LR)

# 1.2
predLR = predict(LR, newdata=test, type='response')
table(test$over50k, predLR > 0.5)
(2123+10421)/nrow(test)

# 1.3
(777+10421)/nrow(test)

# 1.4
library(ROCR)
predROCR = prediction(predLR, test$over50k)
perfROCR = performance(predROCR, 'tpr', 'fpr')
plot(perfROCR)
as.numeric(performance(predROCR, 'auc')@y.values)

# 2.1-2.3
library(rpart)
library(rpart.plot)
CART = rpart(over50k ~ ., data=train, method='class')
prp(CART)

# 2.4
predCART = predict(CART, newdata=test, type='class')
table(test$over50k, predCART)
(10531+1890)/nrow(test)

# 2.5
predCART = predict(CART, newdata=test)
predROCR = prediction(predCART[, 2], test$over50k)
perfROCR = performance(predROCR, 'tpr', 'fpr')
plot(perfROCR)

# 2.6
as.numeric(performance(predROCR, 'auc')@y.values)

# 3.1
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
RF = randomForest(over50k ~ ., data=train)
predRF = predict(RF, newdata=test)
table(test$over50k, predRF)
(11162+1004)/nrow(test)

# 3.2
vu = varUsed(RF, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RF$forest$xlevels[vusorted$ix]))

# 3.3
varImpPlot(RF)

# 4.1
library(caret)
library(e1071)
set.seed(2)
cartGrid = expand.grid(.cp = seq(0.002,0.1,0.002))
numFolds = trainControl(method='cv', number=10)
train(over50k ~ ., data=train, method='rpart', trControl=numFolds, tuneGrid=cartGrid)

# 4.2
CART2 = rpart(over50k ~ ., data=train, method='class', cp=0.002)
predCART2 = predict(CART2, newdata=test, type='class')
table(test$over50k, predCART2)
(10515+2116)/nrow(test)

# 4.3
prp(CART2)
