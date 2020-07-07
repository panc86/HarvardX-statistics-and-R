install.packages("downloader")
install.packages("dplyr")
library(dplyr)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

#' We want to know how close is the average of a random sample to the
#' population average. The central limit theorem helps us answer that.

dat <- na.omit( read.csv(filename) )

# ex1
#' Q
#' If a list of numbers has a distribution that is well approximated
#' by the normal distribution, what proportion of these numbers are
#' within one standard deviation away from the list's average?
#' A
pnorm(1) - pnorm(-1)


# ex2
#' Q
#' What proportion of these numbers are within two standard deviations away
#' from the list's average?
#' A
pnorm(2) - pnorm(-2)


# ex3
#' Q
#' What proportion of these numbers are within three standard deviations
#' away from the list's average?
#' A
pnorm(3) - pnorm(-3)


# ex4
#' Define y to be the weights of males on the control diet.
#' Q
#' What proportion of the mice are within one standard deviation
#' away from the average weight?
#' Remember to use popsd() from rafalib for the population standard deviation.
#' A
#' z-score (also called a standard score) gives you an idea of how far from
#' the mean a data point is. But more technically it’s a measure of how many
#' standard deviations below or above the population mean a raw score is.
#' z = (x – μ) / σ
y <- filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist
z <- (y - mean(y)) / popsd(y)
mean(abs(z) <= 1)


# ex5
#' Q
#' What proportion of these numbers are within two standard
#' deviations away from the list's average?
#' A
mean(abs(z) <= 2)


# ex6
#' Q
#' What proportion of these numbers are within three standard
#' deviations away from the list's average?
#' A
mean(abs(z) <= 3)


# ex7
#' Note that the numbers for the normal distribution and our weights
#' are relatively close. Also, notice that we are indirectly comparing
#' quantiles of the normal distribution to quantiles of the mouse weight
#' distribution. We can actually compare all quantiles using a qqplot.
#' Q
#' Which of the following best describes the qq-plot comparing mouse
#' weights to the normal distribution?
qqnorm(z)
abline(0,1)
# A
#' The mouse weights are well approximated by the normal distribution,
#' although the larger values (right tail) are larger than predicted by the
#' normal. This is consistent with the differences seen between question 3 and 6.


# ex8
#' Create the above qq-plot for the four populations: male/females on each of
#' the two diets.
#' Q
#' What is the best explanation for all these mouse weights
#' being well approximated by the normal distribution?
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
# A
#' This just happens to be how nature behaves in this particular case.
#' Perhaps the result of many biological factors averaging out.


# ex9
#' Here we are going to use the function replicate() to learn about the
#' distribution of random variables. All the above exercises relate to the
#' normal distribution as an approximation of the distribution of a fixed list
#' of numbers or a population. We have not yet discussed probability in these
#' exercises. If the distribution of a list of numbers is approximately normal,
#' then if we pick a number at random from this distribution, it will follow a
#' normal distribution. However, it is important to remember that stating that
#' some quantity has a distribution does not necessarily imply this quantity is
#' random. Also, keep in mind that this is not related to the central limit
#' theorem. The central limit applies to averages of random variables. Let's
#' explore this concept.
#' We will now take a sample of size 25 from the population of males on the
#' chow diet. The average of this sample is our random variable. We will use
#' the replicate() function to observe 10,000 realizations of this random
#' variable. Set the seed at 1, then generate these 10,000 averages. Make a
#' histogram and qq-plot of these 10,000 numbers against the normal distribution.
#' We can see that, as predicted by the CLT, the distribution of the random
#' variable is very well approximated by the normal distribution.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
#' Q
#' What is the average of the distribution of the sample average?
#' A
mean(avgs)


# ex10
#' Q
#' What is the standard deviation of the distribution of sample averages?
#' A
popsd(avgs)
