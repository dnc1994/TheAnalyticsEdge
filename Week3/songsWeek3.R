setwd('C:/Home/Courses/The Analytics Edge/Week3')
Sys.setlocale('LC_ALL', 'C')
songs = read.csv('../data/songs.csv')

# 1.1
str(subset(songs, year==2010))

# 1.2
str(subset(songs, artistname=='Michael Jackson'))

# 1.3
subset(songs, artistname=='Michael Jackson' & Top10==1)

# 1.4
table(songs$timesignature)

# 1.5
songs[which.max(songs$tempo), 2]

# 2.1
songsTrain = subset(songs, year<=2009)
songsTest = subset(songs, year==2010)
str(songsTrain)

# 2.2-2.5
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[, !(names(songsTrain) %in% nonvars) ]
songsTest = songsTest[, !(names(songsTest) %in% nonvars) ]
slog1 = glm(Top10 ~ ., data=songsTrain, family=binomial)
summary(slog1)

# 3.1
cor(songsTrain$energy, songsTrain$loudness)

# 3.2
slog2 = glm(Top10 ~ . - loudness, data=songsTrain, family=binomial)
summary(slog2)

# 3.3
slog3 = glm(Top10 ~ . - energy, data=songsTrain, family=binomial)
summary(slog3)

# 4.1-4.3
predTest = predict(slog3, type='response', newdata=songsTest)
table(songsTest$Top10, predTest > 0.45)
(309 + 19) / (309 + 19 + 5 + 40)
(309 + 5) / (309 + 19 + 5 + 40)

# 4.4
19 / (19 + 40)
309 / (309 + 5)
