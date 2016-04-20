setwd('C:/Home/Courses/The Analytics Edge/Week2')
Sys.setlocale('LC_ALL', 'C')
state = read.csv('../data/statedata.csv')

str(state)

# 1.1
plot(state$x, state$y)

# 1.2
tapply(state$HS.Grad, state$state.region, mean)

# 1.3
boxplot(Murder ~ state.region, data=state)

# 1.4
northeast = subset(state, state.region == 'Northeast')
northeast

# 2.1
lifeExp = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=state)
summary(lifeExp)

# 2.3
plot(state$Income, state$Life.Exp)

# 3.1
summary(lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=state))
summary(lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=state))
lifeExp2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=state)

# 3.3
sort(predict(lifeExp2))
state$state.name[1]
state$state.name[which.min(state$Life.Exp)]

# 3.4
sort(predict(lifeExp2))
state$state.name[47]
state$state.name[which.max(state$Life.Exp)]

state$state.name[which.min(abs(residuals(lifeExp2)))]
state$state.name[which.max(abs(residuals(lifeExp2)))]
