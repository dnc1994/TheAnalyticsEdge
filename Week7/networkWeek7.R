setwd('C:/Home/Courses/The Analytics Edge/Week7')
Sys.setlocale('LC_ALL', 'C')
library(ggplot2)
library(ggmap)
library(maps)

# 1.1
edges = read.csv('../data/edges.csv')
users = read.csv('../data/users.csv')
str(users)
str(edges)
146*2/59

# 1.2
summary(users)
table(users$locale, users$school)

# 1.3
table(users$gender, users$school)

# 2.1
library(igraph)
?graph.data.frame

# 2.2
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

# 2.3
sort(degree(g))

# 2.4
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

# 3.1
V(g)$color = 'black'
V(g)$color[V(g)$gender == 'A'] = 'red'
V(g)$color[V(g)$gender == 'B'] = 'gray'
plot(g, vertex.label=NA)

# 3.2
V(g)$color = 'black'
V(g)$color[V(g)$school == 'A'] = 'red'
V(g)$color[V(g)$school == 'AB'] = 'gray'
plot(g, vertex.label=NA)

# 3.3
V(g)$color = 'black'
V(g)$color[V(g)$locale == 'A'] = 'red'
V(g)$color[V(g)$locale == 'B'] = 'gray'
plot(g, vertex.label=NA)

# 4
?igraph.plotting
