setwd('C:/Home/Courses/The Analytics Edge/Week4')
Sys.setlocale('LC_ALL', 'C')

state = read.csv('../data/statedataSimple.csv')
str(state)

# 1.1
LR = lm(Life.Exp ~ ., data=state)
summary(LR)

# 1.2
predLR = predict(LR)
SSE = sum((predLR - state$Life.Exp)^2)
SSE

# 1.3
LR2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=state)
summary(LR2)

# 1.4
predLR2 = predict(LR2)
SSE = sum((predLR2 - state$Life.Exp)^2)
SSE

# 2.1
library(rpart)
library(rpart.plot)
CART = rpart(Life.Exp ~ ., data=state)
prp(CART)

# 2.2
predCART = predict(CART)
SSE = sum((predCART - state$Life.Exp)^2)
SSE

# 2.3
CART2= rpart(Life.Exp ~ ., data=state, minbucket=5)
prp(CART2)

# 2.5
predCART2 = predict(CART2)
SSE = sum((predCART2 - state$Life.Exp)^2)
SSE

# 2.6
CART3 = rpart(Life.Exp ~ Area, data=state, minbucket=1)
predCART3 = predict(CART3)
SSE = sum((predCART3 - state$Life.Exp)^2)
SSE
prp(CART3)

# 3.1
library(caret)
library(e1071)
set.seed(111)
cartGrid = expand.grid(.cp = seq(0.01,0.50,0.01))
numFolds = trainControl(method='cv', number=10)
train(Life.Exp ~ ., data=state, method='rpart', trControl=numFolds, tuneGrid=cartGrid)

# 3.2
CART4 = rpart(Life.Exp ~ ., data=state, cp=0.12)
prp(CART4)

# 3.3
predCART4 = predict(CART4)
SSE = sum((predCART4 - state$Life.Exp)^2)
SSE

# 3.6
set.seed(111)
cartGrid = expand.grid(.cp = seq(0.01,0.50,0.01))
numFolds = trainControl(method='cv', number=10)
train(Life.Exp ~ Area, data=state, method='rpart', trControl=numFolds, tuneGrid=cartGrid)
CART5 = rpart(Life.Exp ~ Area, data=state, cp=0.02)
prp(CART5)

# 3.7
predCART5 = predict(CART5)
SSE = sum((predCART5 - state$Life.Exp)^2)
SSE
