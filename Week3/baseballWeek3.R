setwd('C:/Home/Courses/The Analytics Edge/Week3')
Sys.setlocale('LC_ALL', 'C')
baseball = read.csv('../data/baseball.csv')

# 1.1
str(baseball)

# 1.2
nrow(table(baseball$Year))

# 1.3
data = subset(baseball, Playoffs==1)
str(data)

# 1.4
table(data$Year)

# 2.1
PlayoffTable = table(data$Year)
names(PlayoffTable)

# 2.2
PlayoffTable[c('1990', '2001')]

# 2.3
data$NumCompetitors = PlayoffTable[as.character(data$Year)]

# 2.4
table(data$NumCompetitors)

# 3.1
data$WorldSeries = as.numeric(data$RankPlayoffs == 1)
table(data$WorldSeries)

# 3.2
model = glm(WorldSeries ~ Year, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RS, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RA, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ W, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ OBP, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ SLG, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ BA, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RankSeason, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ OOBP, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ OSLG, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ NumCompetitors, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ League, data=data, family=binomial)
summary(model)

# 4.1
model = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=data, family=binomial)
summary(model)

# 4.2
cor(data[c('Year', 'RA', 'RankSeason', 'NumCompetitors')])

# 4.3
model = glm(WorldSeries ~ Year, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RA, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RankSeason, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ NumCompetitors, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ Year + RA, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ Year + RankSeason, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ Year + NumCompetitors, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RA + RankSeason, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RA + NumCompetitors, data=data, family=binomial)
summary(model)
model = glm(WorldSeries ~ RankSeason + NumCompetitors, data=data, family=binomial)
summary(model)
