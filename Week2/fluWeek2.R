setwd('C:/Home/Courses/The Analytics Edge/Week2')
Sys.setlocale('LC_ALL', 'C')
train = read.csv('../data/FluTrain.csv')
test = read.csv('../data/FluTest.csv')
library(zoo)

# 1.1
str(train)
train$Week[which.max(train$ILI)]
train$Week[which.max(train$Queries)]

# 1.2
hist(train$ILI)

# 1.3
plot(train$Queries, log(train$ILI))

# 2.2
FluTrend1 = lm(log(ILI) ~ Queries, data=train)
summary(FluTrend1)

# 2.3
cor(log(train$ILI), train$Queries) ^ 2

# 3.1
PredTest1 = exp(predict(FluTrend1, newdata=test))
index = which(test$Week == '2012-03-11 - 2012-03-17')
PredTest1[index]

# 3.2
(test$ILI[index] - PredTest1[index]) / test$ILI[index]

# 3.3
SSE = sum((test$ILI - PredTest1) ^ 2)
RMSE = sqrt(SSE / nrow(test))
RMSE

# 4.1
ILILag2 = lag(zoo(train$ILI), -2, na.pad=TRUE)
train$ILILag2 = coredata(ILILag2)
summary(train$ILILag2)

# 4.2
plot(log(train$ILILag2), log(train$ILI))

# 4.3
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data=train)
summary(FluTrend2)

# 5.1
test$ILILag2 = coredata(lag(zoo(test$ILI), -2, na.pad=TRUE))
summary(test$ILILag2)

# 5.3
n_train = nrow(train)
test$ILILag2[1] = train$ILI[n_train-1]
test$ILILag2[2] = train$ILI[n_train]
test$ILILag2[1]
test$ILILag2[2]

# 5.4
PredTest2 = exp(predict(FluTrend2, newdata=test))
SSE = sum((test$ILI - PredTest2) ^ 2)
RMSE = sqrt(SSE / nrow(test))
RMSE
