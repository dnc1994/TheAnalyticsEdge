setwd('C:/Home/Courses/The Analytics Edge/Week2')
Sys.setlocale('LC_ALL', 'C')
sales = read.csv('../data/elantra.csv')

# 1
train = subset(sales, Year <= 2012)
test = subset(sales, Year > 2012)
str(train)

# 2.1-2.4
model_1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data=train)
summary(model_1)

# 3.1-3.2
model_2 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + Month, data=train)
summary(model_2)

# 3.3
110.69 * 2
110.69 * 4

# 4.1-4.2
model_3 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries + as.factor(Month), data=train)
summary(model_3)

# 5.1-5.2
cor(train)

# 6.1
summary(lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + as.factor(Month), data=train))
model_4 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + as.factor(Month), data=train)

# 6.2
predTest = predict(model_4, newdata=test)
SSE = sum((predTest - test$ElantraSales) ^ 2)
SSE                

# 6.3
SST = sum((mean(train$ElantraSales) - test$ElantraSales) ^ 2)
SST

# 6.4
1 - SSE/SST

# 6.5
max(abs(predTest - test$ElantraSales))

# 6.6
test$Month[which.max(abs(predTest - test$ElantraSales))]
