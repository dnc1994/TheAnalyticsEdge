setwd('C:/Home/Courses/The Analytics Edge/Week3')
Sys.setlocale('LC_ALL', 'C')
loans = read.csv('../data/loans.csv')

# 1.1
table(loans$not.fully.paid)
1533 / (1533 + 8045)

# 1.2
summary(loans)

# 1.3
str(loans)
str(subset(loans, !(is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))))

# 1.4
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), 'not.fully.paid')
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
loans_ = read.csv('../data/loans_imputed.csv')
summary(loans)
summary(loans_)
loans = loans_

# 2.1
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio=0.7)
train = subset(loans, split==T)
test = subset(loans, split==F)
llog1 = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(llog1)

# 2.2
-9.317e-03 * 10
exp(9.317e-03 * 10)

# 2.3
predicted.risk = predict(llog1, newdata=test, type='response')
test$predicted.risk = predicted.risk
table(test$not.fully.paid, predicted.risk > 0.5)
(2400 + 3) / (2400 + 3 + 13 + 457)
(2400) / (2400 + 3 + 13 + 457)

# 2.4
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
ROCRperf = performance(ROCRpred, 'tpr', 'fpr')
as.numeric(performance(ROCRpred, 'auc')@y.values)

# 3.1
llog2 = glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(llog2)

# 3.2
predicted.risk.2 = predict(llog2, newdata=test, type='response')
max(predicted.risk.2)

# 3.3
ROCRpred = prediction(predicted.risk.2, test$not.fully.paid)
ROCRperf = performance(ROCRpred, 'tpr', 'fpr')
as.numeric(performance(ROCRpred, 'auc')@y.values)

# 4.1
10 * exp(0.06 * 3)

# 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit) * 10

# 6.1
highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
mean(highInterest$not.fully.paid)

# 6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
str(selectedLoans)
sum(selectedLoans$profit)
sum(selectedLoans$not.fully.paid)
