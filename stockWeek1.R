setwd('C:/Home/Courses/The Analytics Edge')
Sys.setlocale("LC_ALL", "C")
IBM = read.csv('./data/IBMStock.csv')
GE = read.csv('./data/GEStock.csv')
PG = read.csv('./data/ProcterGambleStock.csv')
CC = read.csv('./data/CocaColaStock.csv')
BE = read.csv('./data/BoeingStock.csv')

# 1.1
IBM$Date = as.Date(IBM$Date,"%m/%d/%y")
GE$Date = as.Date(GE$Date,"%m/%d/%y")
CC$Date = as.Date(CC$Date,"%m/%d/%y")
PG$Date = as.Date(PG$Date,"%m/%d/%y")
BE$Date = as.Date(BE$Date,"%m/%d/%y")
str(IBM)

# 1.2
AllDate = c(IBM$Date, GE$Date, CC$Date, PG$Date, BE$Date)
min(AllDate)

# 1.3
max(AllDate)

# 1.4
mean(IBM$StockPrice)

# 1.5
min(GE$StockPrice)

# 1.6
max(CC$StockPrice)

# 1.7
median(BE$StockPrice)

# 1.8
sd(PG$StockPrice)

# 2.1
plot(CC$Date, CC$StockPrice, type='l')

# 2.2
plot(CC$Date, CC$StockPrice, type='l', col='red')
lines(PG$Date, PG$StockPrice, col='blue', lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

# 2.3
abline(v=as.Date(c("1983-01-01")), lwd=2)

# 3.1-3.2
plot(CC$Date[301:432], CC$StockPrice[301:432], type='l', col='red', ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type='l', col='blue')
lines(GE$Date[301:432], GE$StockPrice[301:432], type='l', col='green')
lines(BE$Date[301:432], BE$StockPrice[301:432], type='l', col='purple')
lines(PG$Date[301:432], PG$StockPrice[301:432], type='l', col='orange')
abline(v=as.Date(c("2000-03-01")), lwd=2)

# 3.3
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-30")), lwd=2)

# 3.4
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)

# 4.1
barplot(tapply(IBM$StockPrice, months(IBM$Date), mean))
abline(h=mean(IBM$StockPrice), lwd=2)

# 4.2-4.3
barplot(tapply(GE$StockPrice, months(GE$Date), mean))
barplot(tapply(CC$StockPrice, months(CC$Date), mean))
