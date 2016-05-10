setwd('C:/Home/Courses/The Analytics Edge/Week5')
Sys.setlocale('LC_ALL', 'C')
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(caTools)
library(randomForest)

wiki = read.csv('../data/wiki.csv', stringsAsFactors=F)
str(wiki)

# 1.1
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

# 1.2

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# 1.3
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# 1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = make.names(colnames(wordsAdded))
colnames(wordsAdded) = paste('A', colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = make.names(colnames(wordsRemoved))
colnames(wordsRemoved) = paste('A', colnames(wordsRemoved))
str(wordsRemoved)

# 1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio=0.7)
train = subset(wikiWords, split==T)
test = subset(wikiWords, split==F)
table(test$Vandal)
(618)/(618+545)


# 1.6
CART = rpart(Vandal ~., data=train, method='class')
predictCART = predict(CART, newdata=test, type='class')
table(test$Vandal, predictCART)
(618+12)/nrow(test)

# 1.7
prp(CART)

# 2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl('http', wiki$Added, fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# 2.2
train2 = subset(wikiWords2, split==T)
test2 = subset(wikiWords2, split==F)
CART2 = rpart(Vandal ~., data=train2, method='class')
predictCART2 = predict(CART2, newdata=test2, type='class')
table(test2$Vandal, predictCART2)
(609+57)/nrow(test2)

# 2.3
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# 2.4
train3 = subset(wikiWords2, split==T)
test3 = subset(wikiWords2, split==F)
CART3 = rpart(Vandal ~., data=train3, method='class')
predictCART3 = predict(CART3, newdata=test3, type='class')
table(test3$Vandal, predictCART3)
(514+248)/nrow(test3)

# 3.1
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
train4 = subset(wikiWords3, split==T)
test4 = subset(wikiWords3, split==F)
CART4 = rpart(Vandal ~., data=train4, method='class')
predictCART4 = predict(CART4, newdata=test4, type='class')
table(test4$Vandal, predictCART4)
(595+241)/nrow(test4)

# 3.2
prp(CART4)
