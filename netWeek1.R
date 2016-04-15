setwd('C:/Home/Courses/The Analytics Edge')
Sys.setlocale("LC_ALL", "C")
Poll = read.csv('./data/AnonymityPoll.csv')

# 1.1
str(Poll)

# 1.2
table(Poll$Smartphone)
summary(Poll)

# 1.3
table(Poll$State, Poll$Region)[,1]
which.max(table(Poll$State, Poll$Region)[,3])

# 2.1
sum(!Poll$Smartphone & !Poll$Internet.Use, na.rm=TRUE)
sum(Poll$Smartphone & Poll$Internet.Use, na.rm=TRUE)
sum(!Poll$Smartphone & Poll$Internet.Use, na.rm=TRUE)
sum(Poll$Smartphone & !Poll$Internet.Use, na.rm=TRUE)

# 2.2
sum(is.na(Poll$Internet.Use))
sum(is.na(Poll$Smartphone))

# 2.3
limited = subset(Poll, Internet.Use | Smartphone)
str(limited)

# 3.1
summary(limited)

# 3.2
mean(limited$Info.On.Internet)

# 3.3
table(limited$Info.On.Internet)

# 3.4
sum(limited$Worry.About.Info, na.rm=TRUE) / sum(!is.na(limited$Worry.About.Info))

# 3.5
sum(limited$Anonymity.Possible, na.rm=TRUE) / sum(!is.na(limited$Anonymity.Possible))

# 3.6
sum(limited$Tried.Masking.Identity, na.rm=TRUE) / sum(!is.na(limited$Tried.Masking.Identity))

# 3.7
sum(limited$Privacy.Laws.Effective, na.rm=TRUE) / sum(!is.na(limited$Privacy.Laws.Effective))

# 4.1
hist(limited$Age)

# 4.2
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

# 4.3
jitter(c(1, 2, 3))

# 4.4
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# 4.5
tapply(limited$Info.On.Internet, limited$Smartphone, mean)

# 4.6
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm=TRUE)
