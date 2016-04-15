setwd('C:/Home/Courses/The Analytics Edge')
Sys.setlocale("LC_ALL", "C")
CPS = read.csv('./data/CPSData.csv')
MAM = read.csv('./data/MetroAreaCodes.csv')
CM = read.csv('./data/CountryCodes.csv')

# 1.1
str(CPS)

# 1.2
barplot(table(CPS$Industry), las=2)
table(CPS$Industry)[which.max(table(CPS$Industry))]

# 1.3
sort(table(CPS$State))

# 1.4
table(CPS$Citizenship) / nrow(CPS)
0.88832615 + 0.05386818

# 1.5
table(CPS$Race, CPS$Hispanic)

# 2.1
summary(CPS)

# 2.2
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# 2.3
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[,2] != 0 & table(CPS$State, is.na(CPS$MetroAreaCode))[,1] == 0)
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[,1] != 0 & table(CPS$State, is.na(CPS$MetroAreaCode))[,2] == 0)

# 2.4
CPS$NonMetro = is.na(CPS$MetroAreaCode)
table(CPS$NonMetro, CPS$Region) / nrow(CPS)

# 2.5
sort(tapply(CPS$NonMetro, CPS$State, mean))

# 3.1
str(MAM)
str(CM)

# 3.2
CPS = merge(CPS, MAM, by.x='MetroAreaCode', by.y='Code', all.x=TRUE)
str(CPS)
summary(CPS$MetroArea)

# 3.3
sort(table(CPS$MetroArea))

# 3.4
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# 3.5
sum(tapply(CPS$Race=='Asian', CPS$MetroArea, mean) >= 0.2, na.rm=TRUE)

# 3.6
sort(tapply(CPS$Education=="No high school diploma", CPS$MetroArea, mean, na.rm=TRUE), decreasing=TRUE)

# 4.1
CPS = merge(CPS, CM, by.x='CountryOfBirthCode', by.y='Code', all.x=TRUE)
str(CPS)
summary(CPS$Country)

# 4.2
sort(table(CPS$Country))

# 4.3
tapply(CPS$Country!='United States', CPS$MetroArea, mean, na.rm=TRUE)['New York-Northern New Jersey-Long Island, NY-NJ-PA']

# 4.4
sort(tapply(CPS$Country=='India', CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=='Brazil', CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country=='Somalia', CPS$MetroArea, sum, na.rm=TRUE))
