setwd('C:/Home/Courses/The Analytics Edge/Week5')
Sys.setlocale('LC_ALL', 'C')
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)
library(ROCR)

# 1.1
emails = read.csv('../data/emails.csv', stringsAsFactors=F)
str(emails)

# 1.2
table(emails$spam)

# 1.5
max(nchar(emails$text))

# 1.6
which.min(nchar(emails$text))

# 2.1
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

# 2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# 2.3
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
colSums(emailsSparse)[which.max(colSums(emailsSparse))]

# 2.4
emailsSparse$spam = emails$spam
hamColSums = colSums(subset(emailsSparse, spam==F))
hamColSums[hamColSums > 5000]

# 2.5
spamColSums = colSums(subset(emailsSparse, spam==T))
spamColSums[spamColSums > 1000]

# 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio=0.7)
train = subset(emailsSparse, split==T)
test = subset(emailsSparse, split==F)
spamLog = glm(spam ~ ., data=train, family=binomial)
spamCART = rpart(spam ~., data=train, method='class')
spamRF = randomForest(spam ~ ., data=train)
predTrainLog = predict(spamLog, type='response')
predTrainCART = predict(spamCART)[, 2]
predTrainRF = predict(spamRF, type='prob')[, 2]
sum(predTrainLog < 0.00001)
sum(predTrainLog > 0.99999)
length(predTrainLog) - 3046 - 954

# 3.2
summary(spamLog)

# 3.3
prp(spamCART)

# 3.4
table(train$spam, predTrainLog > 0.5)
(3052+954)/nrow(train)

# 3.5
predROCRLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predROCRLog, 'auc')@y.values)

# 3.6
table(train$spam, predTrainCART > 0.5)
(2885+894)/nrow(train)

# 3.7
predROCRCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predROCRCART, 'auc')@y.values)

# 3.8
table(train$spam, predTrainRF > 0.5)
(3016+918)/nrow(train)

# 3.9
predROCRRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predROCRRF, 'auc')@y.values)

# 4.1
predTestLog = predict(spamLog, newdata=test, type='response')
predTestCART = predict(spamCART, newdata=test)[, 2]
predTestRF = predict(spamRF, newdata=test, type='prob')[, 2]
table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test)

# 4.2
as.numeric(performance(prediction(predTestLog, test$spam), 'auc')@y.values)

# 4.3
table(test$spam, predTestCART > 0.5)
(1228+386)/nrow(test)

# 4.4
as.numeric(performance(prediction(predTestCART, test$spam), 'auc')@y.values)

# 4.5
table(test$spam, predTestRF > 0.5)
(1292+389)/nrow(test)

# 4.6
as.numeric(performance(prediction(predTestRF, test$spam), 'auc')@y.values)

# 6.1
wordCount = rowSums(as.matrix(dtm))

# 6.2
hist(wordCount)

# 6.3
hist(log(wordCount))

# 6.4
emailsSparse$logWordCount = log(wordCount)
boxplot(logWordCount ~ spam, data=emailsSparse)

# 6.5
train2 = subset(emailsSparse, split==T)
test2 = subset(emailsSparse, split==F)
spam2CART = rpart(spam ~ ., data=train2, method='class')
set.seed(123)
spam2RF = randomForest(spam ~., data=train2)
prp(spam2CART)

# 6.6
predTest2CART = predict(spam2CART, newdata=test2)[, 2]
table(test2$spam, predTest2CART > 0.5)
(1214+394)/nrow(test2)

# 6.7
as.numeric(performance(prediction(predTest2CART, test2$spam), 'auc')@y.values)

# 6.8
predTest2RF = predict(spam2RF, newdata=test2, type='prob')[, 2]
table(test2$spam, predTest2RF > 0.5)
(1298+382)/nrow(test2)

# 6.9
as.numeric(performance(prediction(predTest2RF, test2$spam), 'auc')@y.values)
