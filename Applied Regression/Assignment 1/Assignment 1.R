############################################################
####################ASSIGNMENT-1############################
############################################################
#Problem-1
library(alr4)
head(UN11)
plot(fertility~ppgdp,data = UN11)

plot(log(fertility)~log(ppgdp),data = UN11)
lm(log(fertility)~log(ppgdp),data = UN11)
abline(lm(log(fertility)~log(ppgdp),data = UN11))

#problem-3
plot(Temp~Month,Mitchell)
lm(Temp~Month,Mitchell)
abline(lm(Temp~Month,Mitchell))

library(tidyverse)

ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)


ggplot(aes(x = Month, y = Temp), data = Mitchell) +
  geom_point() +
  geom_line()

#problem-4
head(oldfaith)
plot(Interval~Duration,oldfaith)
lm(Interval~Duration,oldfaith)
abline(lm(Interval~Duration,oldfaith))

oldfaith$Interval
hist(oldfaith$Interval)
summary(oldfaith$Interval)

#problem-2
head(wblake)
plot(Length~Age,wblake)
lm(Length~Age,wblake)
abline(lm(Length~Age,wblake))
var(wblake$Length[wblake$Age==1])
var(wblake$Length[wblake$Age==2])
var(wblake$Length[wblake$Age==3])


