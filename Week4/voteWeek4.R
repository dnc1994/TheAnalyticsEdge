setwd('C:/Home/Courses/The Analytics Edge/Week4')
Sys.setlocale('LC_ALL', 'C')

votes = read.csv('../data/gerber.csv')
str(votes)

# 1.1
mean(votes$voting)

# 1.2
summary(subset(votes, voting==1))

# 1.3
lr = glm(voting ~ hawthorne + civicduty + neighbors + self, data=votes, family=binomial)
summary(lr)

# 1.4
predLR = predict(lr, type='response')
table(votes$voting, predLR > 0.3)
(134513+51966)/(134513+51966+100875+56730)

# 1.5
table(votes$voting, predLR > 0.5)
(235388)/(134513+51966+100875+56730)

# 2.1
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=votes)
prp(CARTmodel)

# 2.2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=votes, cp=0.0)
prp(CARTmodel2)

# 2.4
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=votes, cp=0.0)
prp(CARTmodel3)

# 3.1
T1 = rpart(voting ~ control, data=votes, cp=0.0)
prp(T1, digits=6)
abs(0.296638-0.34)

# 3.2
T2 = rpart(voting ~ control + sex, data=votes, cp=0.0)
prp(T2, digits=6)
abs(0.290456-0.334176)
abs(0.302795-0.345818)

# 3.3
LR2 = glm(voting ~ sex + control, data=votes, family=binomial)
summary(LR2)

# 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LR2, newdata=Possibilities, type='response')
predict(T2, newdata=Possibilities)
abs(0.2908065-0.2904558 )

# 3.5
LR3 = glm(voting ~ sex + control + sex:control, data=votes, family='binomial')
summary(LR3)

# 3.6
predict(LR3, newdata=Possibilities, type='response')
abs(0.2904558-0.2904558)
