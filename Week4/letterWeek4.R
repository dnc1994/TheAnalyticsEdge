setwd('C:/Home/Courses/The Analytics Edge/Week4')
Sys.setlocale('LC_ALL', 'C')

letters = read.csv('../data/letters_ABPR.csv')
str(letters)

# 1.1
letters$isB = as.factor(letters$letter=='B')
set.seed(1000)
split = sample.split(letters, SplitRatio=0.5)
train = subset(letters, split==T)
test = subset(letters, split==F)
1 - mean(letters$letter=='B')

# 1.2
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method='class')
predCARTb = predict(CARTb, newdata=test, type='class')
table(test$isB, predCARTb)
(1136+319)/(1136+319+70+33)

# 1.3
library(randomForest)
RFb = randomForest(isB ~ . - letter, data=train)
predRFb = predict(RFb, newdata=test, type='class')
table(test$isB, predRFb)
(1161+365)/(1136+319+70+33)

# 2.1
letters$letter = as.factor(letters$letter)
table(letters$letter) / nrow(letters)

# 2.2
CART = rpart(letter ~ . - isB, data=train, method='class')
predCART = predict(CART, newdata=test, type='class')
table(test$letter, predCART)
(344+295+381+336)/nrow(test)

# 2.3
RF = randomForest(letter ~ . - isB, data=train)
predRF = predict(RF, newdata=test, type='class')
table(test$letter, predRF)
(372+377+390+387)/nrow(test)
