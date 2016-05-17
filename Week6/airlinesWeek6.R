setwd('C:/Home/Courses/The Analytics Edge/Week6')
Sys.setlocale('LC_ALL', 'C')

airlines = read.csv('../data/AirlinesCluster.csv')
str(airlines)

# 1.1
summary(airlines)

# 1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

# 2.1
distances = dist(airlinesNorm, method = 'euclidean')
hierClusters = hclust(distances, method = 'ward.D')
plot(hierClusters)

# 2.2
clusterGroups = cutree(hierClusters, k = 5)
table(clusterGroups)

# 2.3-2.7
tapply(airlinesNorm$Balance, clusterGroups, mean)
tapply(airlinesNorm$QualMiles, clusterGroups, mean)
tapply(airlinesNorm$BonusMiles, clusterGroups, mean)
tapply(airlinesNorm$BonusTrans, clusterGroups, mean)
tapply(airlinesNorm$FlightMiles, clusterGroups, mean)
tapply(airlinesNorm$FlightTrans, clusterGroups, mean)
tapply(airlinesNorm$DaysSinceEnroll, clusterGroups, mean)

# 3.1
set.seed(888)
clusterGroups2 = kmeans(airlinesNorm, centers=5, iter.max=1000)
table(clusterGroups2$cluster)
