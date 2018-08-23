library(ggplot2)
?ggplot2
?ggplot
?diamonds

?scatter.smooth

ggplot(aes(x = price, y = carat), data = diamonds) +
  scatter.smooth()

ggplot(data = diamonds, aes(x = x, y = price)) +
  geom_point()

cor(diamonds$price, diamonds$x)
cor(diamonds$price, diamonds$y)

?cor

qplot(x = price, y = depth, data = diamonds)

?alpha

?ggplot

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100)
scale_x_continuous(breaks = 2)

cor(diamonds$depth, diamonds$price)

?range

range(diamonds$price)

quantile(diamonds$price, .99)
quantile(diamonds$carat, .99)

ggplot(data = diamonds, aes(x = price, y = carat)) +
  geom_point()
  xlim(0 ,17378.22)
  ylim(0 ,2.18)
  
diamonds$volume <- diamonds$x*diamonds$y*diamonds$z

ggplot(data = diamonds, aes(x = price, y = volume)) +
  geom_point()

cor(diamonds$price, diamonds$volume)

volume_no.outliers <- where(volume<=800 & volume>0)

?subset

volume_no.outliers <- subset(diamonds, volume > 0 & volume <= 800)

range(diamonds$volume)

range(volume_no.outliers$volume)

cor(volume_no.outliers$volume, volume_no.outliers$price)

volume_no.outliers <- subset(diamonds, volume > 0 & volume <= 800)
ggplot(data = volume_no.outliers, aes(x = price, y = volume)) +
  geom_point(alpha = 1/100)
  geom_smooth(method = "lm", formula = y ~ x, size = 1)

?geom_smooth
  
library(dplyr)

?dplyr
  
vignette("dplyr")

diamondsByClarity <- mutate(diamonds, mean_price = mean(price), median_price = median(price), min_price = min(price), max_price = max(price), n = diamonds$clarity)
  
head(diamondsByClarity)

diamondsByClarity <- diamonds %>% group_by(clarity) %>% summarise(mean_price=mean(price),median_price=median(as.numeric(price)),
                                                               min_price=min(price),max(price),n=n())