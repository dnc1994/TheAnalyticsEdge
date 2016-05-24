setwd('C:/Home/Courses/The Analytics Edge/Week7')
Sys.setlocale('LC_ALL', 'C')
library(ggplot2)
library(ggmap)
library(maps)

statesMap = map_data('state')

# 1.1
str(statesMap)
table(statesMap$group)

# 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill ='white', color ='black')

# 2.1
polling = read.csv('../data/PollingImputed.csv')
str(polling)
Train = subset(polling, Year <= 2008)
Test = subset(polling, Year > 2008)
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family='binomial')
TestPrediction = predict(mod2, newdata=Test, type='response')
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
str(predictionDataFrame)
table(predictionDataFrame$TestPredictionBinary)
mean(predictionDataFrame$TestPrediction)

# 2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by ='region')
predictionMap = predictionMap[order(predictionMap$order), ]
str(predictionMap)
str(statesMap)


# 2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color ='black')

# 2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color ='black') +
  scale_fill_gradient(low ='blue', high ='red', guide ='legend', breaks= c(0,1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')


ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color ='black') +
  scale_fill_gradient(low ='blue', high ='red', guide ='legend', breaks= c(0,1), labels = c('Democrat', 'Republican'), name = 'Prediction 2012')

# 3.2
TestPrediction[Test$State == 'Florida']

# 4
?geom_polygon
