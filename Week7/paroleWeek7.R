setwd('C:/Home/Courses/The Analytics Edge/Week7')
Sys.setlocale('LC_ALL', 'C')
library(ggplot2)

parole = read.csv('../data/parole.csv')
str(parole)

# 1.1
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
table(parole$male)
130 / nrow(parole)

# 1.2
table(parole$state, parole$crime)

# 2.1
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)

# 2.2
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color='blue')

# 3.1
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

# 3.2
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(. ~ male)

# 3.3
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

# 3.4
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) +
  geom_histogram(binwidth = 5,  position='identity', alpha=0.5) + scale_fill_manual(values=colorPalette)

# 4.1
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1)

# 4.2
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1)

# 4.3
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(crime ~ .)
