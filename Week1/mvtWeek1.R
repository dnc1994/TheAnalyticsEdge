setwd('C:/Home/Courses/The Analytics Edge')
Sys.setlocale("LC_ALL", "C")
mvt = read.csv('./data/mvtWeek1.csv')

# 1.1-1.2
summary(mvt)

# 1.3
max(mvt$ID)

# 1.4
min(mvt$Beat)

# 1.5
table(mvt$Arrest)

# 1.6
str(subset(mvt, LocationDescription=="ALLEY"))

# 2.1
mvt$Date[0]

# 2.2
DateConverted = as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))
summary(DateConverted)

# 2.3
mvt$Month = months(DateConverted)
mvt$Weekday = weekdays(DateConverted)
mvt$Date = DateConverted
table(mvt$Month)

# 2.4
table(mvt$Weekday)

# 2.5
table(subset(mvt, Arrest==TRUE)$Month)

# 3.1
hist(mvt$Date, breaks=100)

# 3.2
boxplot(mvt$Date ~ mvt$Arrest)

# 3.3
table(subset(mvt, Year=="2001")$Arrest)
2152 / (2152 + 18517)

# 3.4
table(subset(mvt, Year=="2007")$Arrest)
1212 / (1212 + 13068)

# 3.5
table(subset(mvt, Year=="2012")$Arrest)
550 / (550 + 13542)

# 4.1
sort(table(mvt$LocationDescription))

# 4.2
Top5 = subset(mvt, LocationDescription == 'STREET' |
                   LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' |
                   LocationDescription == 'ALLEY' | 
                   LocationDescription == 'GAS STATION' | 
                   LocationDescription == 'DRIVEWAY - RESIDENTIAL'
             )
str(Top5)

# 4.3
Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)
ArrestRate <- function(column) {
  return(sum(column) / length(column));
}
tapply(Top5$Arrest, Top5$LocationDescription, ArrestRate)

# 4.4
GAS = subset(Top5, LocationDescription == 'GAS STATION')
barplot(table(GAS$Weekday))

# 4.5
DRI = subset(Top5, LocationDescription == 'DRIVEWAY - RESIDENTIAL')
barplot(table(DRI$Weekday))
