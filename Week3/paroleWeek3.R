setwd('C:/Home/Courses/The Analytics Edge/Week3')
Sys.setlocale('LC_ALL', 'C')
parole = read.csv('../data/parole.csv')

# 1.1, 2.1
str(parole)

# 1.2
table(parole$violator)

# 2.2
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

# 3.1
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio=0.7)
train = subset(parole, split==T)
test = subset(parole, split==F)

# 4.1-4.2
plog1 = glm(violator ~ ., data=train, family=binomial)
summary(plog1)

# 4.3
logodds = 0.3869904 + 0.8867192 -0.0001756 * 50 -0.1238867 * 3 + 0.0802954 * 12 + 0.6837143 -4.2411574
exp(logodds)
exp(logodds) / (1 + exp(logodds))

# 5.1
predTest = predict(plog1, type='response', newdata=test)
max(predTest)

# 5.2
table(test$violator, predTest > 0.5)
12 / (11 + 12)
167 / (167 + 12)
(167 + 12) / (167 + 12 + 11 + 12)

# 5.3
(167 + 12) / (167 + 12 + 11 + 12)

# 5.6
library(ROCR)
ROCRpred = prediction(predTest, test$violator)
ROCRperf = performance(ROCRpred, 'tpr', 'fpr')
as.numeric(performance(ROCRpred, 'auc')@y.values)
