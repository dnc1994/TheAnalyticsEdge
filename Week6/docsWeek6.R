setwd('C:/Home/Courses/The Analytics Edge/Week6')
Sys.setlocale('LC_ALL', 'C')

doc = read.csv('../data/dailykos.csv')
str(doc)

# 1.1
distances = dist(doc, method = 'euclidean')
clusterDoc = hclust(distances, method = 'ward.D')

# 1.2-1.3
plot(clusterDoc)

# 1.4
clusterGroups = cutree(clusterDoc, k = 7)
table(clusterGroups)

# 1.5
HierCluster1 = subset(doc, clusterGroups == 1)
tail(sort(colMeans(HierCluster1)))

# 1.6
tail(sort(colMeans(subset(doc, clusterGroups == 2))))
tail(sort(colMeans(subset(doc, clusterGroups == 3))))
tail(sort(colMeans(subset(doc, clusterGroups == 4))))
tail(sort(colMeans(subset(doc, clusterGroups == 5))))
tail(sort(colMeans(subset(doc, clusterGroups == 6))))
tail(sort(colMeans(subset(doc, clusterGroups == 7))))

# 2.1
set.seed(1000)
clusterGroups2 = kmeans(doc, centers=7)
clusterGroups2
table(clusterGroups2$cluster)

# 2.2
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 1))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 2))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 3))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 4))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 5))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 6))))
tail(sort(colMeans(subset(doc, clusterGroups2$cluster == 7))))

# 2.3-2.6
table(clusterGroups, clusterGroups2$cluster)
