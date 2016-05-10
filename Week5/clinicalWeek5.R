setwd('C:/Home/Courses/The Analytics Edge/Week5')
Sys.setlocale('LC_ALL', 'C')
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)

trials = read.csv('../data/clinical_trial.csv', stringsAsFactors=F)
str(trials)

# 1.1
max(nchar(trials$abstract))

# 1.2
sum(nchar(trials$abstract) == 0)

# 1.3
trials[which.min(nchar(trials$title)), 'title']

# 2.1
corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords('english'))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
wordsTitle = as.data.frame(as.matrix(sparseTitle))
colnames(wordsTitle) = make.names(colnames(wordsTitle))
str(wordsTitle)

corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords('english'))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)
wordsAbstract = as.data.frame(as.matrix(sparseAbstract))
colnames(wordsAbstract) = make.names(colnames(wordsAbstract))
str(wordsAbstract)

# 2.3
colSums(wordsAbstract)[which.max(colSums(wordsAbstract))]

# 3.1
colnames(wordsTitle) = paste0('T', colnames(wordsTitle))
colnames(wordsAbstract) = paste0('A', colnames(wordsAbstract))

# 3.2
words = cbind(wordsTitle, wordsAbstract)
words$trial = trials$trial
str(words)

# 3.3
set.seed(144)
split = sample.split(words$trial, SplitRatio=0.7)
train = subset(words, split==T)
test = subset(words, split==F)
table(test$trial)
(313)/(313+245)

# 3.4
CART = rpart(trial ~., data=train, method='class')
prp(CART)

# 3.5
predictTrain = predict(CART)[, 2]
max(predictTrain)

# 3.7
table(train$trial, predictTrain > 0.5)
(631+441)/nrow(train)
(441)/(441+131)
(631)/(631+99)

# 4.1
predictTest = predict(CART, newdata=test)[, 2]
table(test$trial, predictTest > 0.5)
(261+162)/nrow(test)

# 4.2
library(ROCR)
predROCR = prediction(predictTest, test$trial)
perfROCR = performance(predROCR, 'tpr', 'fpr')
as.numeric(performance(predROCR, 'auc')@y.values)
