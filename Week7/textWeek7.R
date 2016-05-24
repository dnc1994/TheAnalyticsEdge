setwd('C:/Home/Courses/The Analytics Edge/Week7')
Sys.setlocale('LC_ALL', 'C')
library(tm)

tweets = read.csv('../data/tweets.csv', stringsAsFactors=F)
str(tweets)

# 1.1
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords('english'))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
dtm

# 2.1-2.2
library(wordcloud)
?wordcloud

# 2.3
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# 2.4
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords('english'), 'apple'))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

# 4.1-4.2
library(RColorBrewer)
?brewer.pal
display.brewer.all()

# 4.4
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, 'Blues'))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25), colors=brewer.pal(9, 'Blues')[c(-1,-2,-3,-4)])
