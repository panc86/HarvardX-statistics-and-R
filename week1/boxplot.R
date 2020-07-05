# ex1
#' Q
#' 
#' A
head(InsectSprays)
boxplot(count~spray, data=InsectSprays)
groups <- split(InsectSprays, InsectSprays$spray)
median(groups$A$count)
median(groups$B$count)
median(groups$C$count)
median(groups$D$count)
median(groups$E$count)
median(groups$F$count)


# ex2
#' Q
#' 
library(dplyr)
data(nym.2002, package="UsingR")
time_by_gender <- split(nym.2002$time, nym.2002$gender)
males <- time_by_gender$Male
females <- time_by_gender$Female
par(mfrow=c(1, 3))
boxplot(females, males)
hist(females,xlim=c(range(nym.2002$time)))
hist(males,xlim=c(range(nym.2002$time)))

#' A
#' Male and females have similar right skewed distributions with the former,
#' 20 minutes shifted to the left.
mean(males) - mean(females)
