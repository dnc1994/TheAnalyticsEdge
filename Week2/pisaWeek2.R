setwd('C:/Home/Courses/The Analytics Edge/Week2')
Sys.setlocale('LC_ALL', 'C')
train = read.csv('../data/pisa2009train.csv')
test = read.csv('../data/pisa2009test.csv')

# 1.1
str(train)

# 1.2
tapply(train$readingScore, train$male, mean)

# 1.3
summary(train)

# 1.4
train = na.omit(train)
test = na.omit(test)
str(train)
str(test)

# 2.1
str(train)

# 3.1
train$raceeth = relevel(train$raceeth, 'White')
test$raceeth = relevel(test$raceeth, 'White')
lmScore = lm(readingScore ~ ., data=train)
summary(lmScore)

# 3.2
predictTrain = predict(lmScore, newdata=train)
SSE = sum((predictTrain - train$readingScore) ^ 2)
RMSE = sqrt(SSE / nrow(train))
RMSE

# 3.3
summary(lmScore)
29.542707 * 2

# 4.1
predictTest = predict(lmScore, newdata=test)
max(predictTest) - min(predictTest)

# 4.2
SSE = sum((predictTest - test$readingScore) ^ 2)
SSE
RMSE = sqrt(SSE / nrow(test))
RMSE

# 4.3
baseline = mean(train$readingScore)
baseline
SST = sum((baseline - test$readingScore) ^ 2)
SST

# 4.4
1 - SSE/SST
