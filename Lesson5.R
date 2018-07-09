library(ggplot2)
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20) +
  xlim(13, 90) +
  coord_trans(y = "sqrt")

?coord_trans()

ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/20) +
  xlim(13, 90) +
  coord_trans(y = "sqrt")

library(dplyr)

age_groups <- group_by(pf, age)

pf.fc_by_age <- summarise(age_groups, 
                          friend_count_mean = mean(friend_count),
                          friend_count_median = median(friend_count),
                          n = n())

pf.fc_by_age <- arrange(pf.fc_by_age, age)

head(pf.fc_by_age)

ggplot(aes(x = age, y = friend_count_mean), data = pf.fc_by_age) +
  geom_line()

ggplot(aes(x = age, y = friend_count), data = pf) +
  xlim(13, 90) +
  geom_point(alpha = 0.05,
             position = position_jitter(h = 0),
             color = 'orange') +
  coord_trans(y = 'sqrt') +
  geom_line(stat = 'summary', fun.y = mean) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1),
            linetype = 2, color = 'blue') +
  geom_line(stat = 'summary', fun.y = median,
            color = 'red') 
  

?fun.y
??fun.y
?geom_line
?quantile
?summary
?fun

?cor.test

cor.test(x = age, y = friend_count, data = pf)

x <- pf$age
y <- pf$friend_count

cor.test(x, y)

with(pf, cor.test(age, friend_count, method = 'pearson'))

with(subset(pf, age <= 70), cor.test(age, friend_count))

?subset