install.packages("gapminder")
library(dplyr)
library(gapminder)
data(gapminder)
head(gapminder)

#' Create a vector x of the life expectancy of each
#' country for the year 1952. Plot a histogram of these
#' life expectancy to see the spread of the different countries.

x <- filter(gapminder, year==1952) %>% select(lifeExp) %>% unlist
hist(x)


# ex1
#' In statistics, the empirical cumulative distribution function
#' (or empirical cdf or empirical distribution function) is the function
#' F(a) for any a, which tells you the proportion of the values which are
#' less than or equal to a.
#' Q
#' What is the proportion of countries in 1952 that have a life expectancy
#' less than or equal to 40?
#' A
a <- 40
mean(x <= a)


# ex2
#' Q
#' What is the proportion of countries in 1952 that have a life expectancy
#' between 40 and 60 years?
#' Hint: this is the proportion that have a life expectancy less than or
#' equal to 60 years, minus the proportion that have a life expectancy
#' less than or equal to 40 years.
#' A
mean(x <= 60) - mean(x <= 40)

#' Q
#' plot the proportions of countries with life expectancy q for a range
#' of different years
#' A
plot(ecdf(x))

#' But suppose we didn't know this. The function is quite easy to build
#' by turning `mean(x <= a)` into a custom function, and then using `sapply()`.
#' Our custom function will take an input variable q, and return the proportion
#' of countries in x less than or equal to q.
prop = function(q) {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
# or "inline" function or an "anonymous" function
# props = sapply(qs, function(q) mean(x <= q))
plot(qs, props)
