setwd('C:/Home/Courses/The Analytics Edge/Week2')
Sys.setlocale('LC_ALL', 'C')
climate = read.csv('../data/climate_change.csv')
str(climate)

# 1.1-1.2
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
model_full = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model_full)

# 2.1
cor(train$N2O, train$CFC.11)
plot(train$N2O, train$CFC.11)
model_2 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.12 + TSI + Aerosols, data=train)
summary(model_2)

# 2.2
cor(train)

# 3
model_3 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train)
summary(model_3)

# 4
sim_model = step(model_full)
summary(sim_model)

# 5
predictions = predict(sim_model, newdata=test)
SSE = sum((predictions - test$Temp) ^ 2)
SST = sum((mean(train$Temp) - test$Temp) ^ 2)
1 - SSE/SST
