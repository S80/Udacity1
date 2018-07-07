library(ggplot2)
data(diamonds)
summary(diamonds)
?diamonds
?ggplot
?qplot
qplot(price, data = diamonds)
?ifelse
x <- ifelse(diamonds$price < 500, 1, 0)
sum(x)
y <- ifelse(diamonds$price < 250, 1, 0)
sum(y)
z <- ifelse(diamonds$price >= 15000, 1, 0)
sum(z)
qplot(x = price, data = diamonds, binwidth = 10) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50))
?grid
?qplot
?hist
hist(diamonds$price[diamonds$cut])
qplot(x = diamonds$cut, y = diamonds$price)
qplot(x = cut, y = price, data = diamonds)
qplot(x = cut, data = diamonds) +
  scale_x_discrete(breaks = 1:5)     
  facet_wrap( ~ price, ncol = 5)
  
D <- qplot(price, data = diamonds)


qplot(price, data = diamonds) +
  facet_wrap( ~ cut, ncol=5)

summary(diamonds) +
  facet_wrap( ~ cut, ncol=5)

summary(subset(diamonds))

q <- (diamonds$price = "fair")

library(psych)

install.packages("psych")

describeBy(diamonds, group=diamonds$cut)

Dimz <- subset(diamonds, !is.na(price))

describeBy(Dimz, group=diamonds$cut)

Dimz <- na.omit(diamonds)

?na.omit

na.omit(diamonds)

summary(diamonds)

Dimz <- diamonds
na.omit(Dimz)

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free")

?facet_wrap

qplot(price/carat, data = diamonds, binwidth = 1) +
  scale_x_log10() +
  facet_wrap( ~ cut, ncol=5)

?boxplot

boxplot(diamonds$price) 
summary(diamonds$price)
describeBy(diamonds$price, group=diamonds$color)

IQR(diamonds$price)

?IQR

?summary
IQR(diamonds$price, group=diamonds$color)

IQR(subset(diamonds, price <1000)$price)

summary(diamonds$price) +
  facet_wrap( ~ color, ncol = 7)


?diamonds
?by

by(diamonds$price, diamonds$color, IQR)

by(diamonds$price, diamonds$color, summary)


AlcCons <- read.csv('Alcohol.csv')

qplot(2005, data = Alcohol_csv)

qplot(x = Alcohol_csv$X__1$France, y = Alcohol_csv$"2005")

Q <- qplot(x = IntlGiving$`ODA % GNI`, y = IntlGiving$`2008`)

?qplot

?xlab

Q + theme(axis.text.x=element_text(angle = 90))
