install.packages("downloader")
install.packages("rafalib")
install.packages("dplyr")
library(dplyr)
library(rafalib)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)
dat <- read.csv(filename)
head(dat)


# ex1
#' Suppose we are interested in the proportion of times we see a 6 when rolling
#' n=100 dice. This is a random variable which we can simulate with
#' x=sample(1:6, n, replace=TRUE) and the proportion we are interested in can
#' be expressed as an average: mean(x==6). Because the die rolls are
#' independent, the CLT applies.
#' We want to roll n dice 10,000 times and keep these proportions. This random
#' variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n. So
#' according to the CLT, z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be
#' normal with mean 0 and SD 1.
#' Q
#' Set the seed to 1, then use replicate() to perform the simulation, and
#' report what proportion of times z was larger than 2 in absolute value
#' (CLT says it should be about 0.05)
#' A
seed.set(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE)
  z <- (mean(x == sides) - p) / sqrt(p * (1 - p) / n)
})
qqnorm(zs)
# confirm it's well approximated with normal distribution
abline(0,1)
mean(abs(zprop) > 2)


# ex2
#' For the last simulation you can make a qqplot to confirm the normal
#' approximation. Now, the CLT is an asymptotic result, meaning it is closer
#' and closer to being a perfect approximation as the sample size increases.
#' In practice, however, we need to decide if it is appropriate for actual
#' sample sizes. Is 10 enough? 15? 30?
#' In the example used in exercise 1, the original data is binary (either 6 or
#' not). In this case, the success probability also affects the appropriateness
#' of the CLT. With very low probabilities, we need larger sample sizes for the
#' CLT to "kick in".
#' Run the simulation from exercise 1, but for different values of p and n.
#' Q
#' For which of the following is the normal approximation best?
#' A
mypar(2, 2)
# test1
seed.set(1)
n <- 5
sides <- 2 # (p=0.5)
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE)
  z <- (mean(x == sides) - p) / sqrt(p * (1 - p) / n)
})
qqnorm(zs, main = "p=0.5, n=5")
# confirm it's well approximated with normal distribution
abline(0,1)
mean(abs(zprop) > 2)
# test2
seed.set(1)
n <- 30
sides <- 2 # (p=0.5)
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE)
  z <- (mean(x == sides) - p) / sqrt(p * (1 - p) / n)
})
qqnorm(zs, main = "p=0.5, n=30")
# confirm it's well approximated with normal distribution
abline(0,1)
mean(abs(zprop) > 2)
# test3
seed.set(1)
n <- 30
sides <- 100 # (p=0.01)
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE)
  z <- (mean(x == sides) - p) / sqrt(p * (1 - p) / n)
})
qqnorm(zs, main = "p=0.01, n=30")
# confirm it's well approximated with normal distribution
abline(0,1)
mean(abs(zprop) > 2)
# test4
seed.set(1)
n <- 100
sides <- 100 # (p=0.01)
p <- 1/sides
zs <- replicate(10000, {
  x <- sample(1:sides, n, replace=TRUE)
  z <- (mean(x == sides) - p) / sqrt(p * (1 - p) / n)
})
qqnorm(zs, main = "p=0.01, n=100")
# confirm it's well approximated with normal distribution
abline(0,1)
mean(abs(zprop) > 2)


# ex3
#' As we have already seen, the CLT also applies to averages of quantitative data.
#' A major difference with binary data, for which we know the variance is `p(1−p)`,
#' is that with quantitative data we need to estimate the population standard
#' deviation. In several previous exercises we have illustrated statistical
#' concepts with the unrealistic situation of having access to the entire
#' population. In practice, we do *not* have access to entire populations.
#' Instead, we obtain one random sample and need to reach conclusions analyzing
#' that data. `dat` is an example of a typical simple dataset representing just
#' one sample. We have 12 measurements for each of two populations:
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
#' We think of X as a random sample from the population of all mice in the
#' control diet and Y as a random sample from the population of all mice in
#' the high fat diet.
#' Define the parameter μX as the average of the control population.
#' We estimate this parameter with the sample average X¯.
#' Q
#' What is the sample average?
#' A
mu_x <- mean(X)


# ex4
#' We don't know μX (population mean), but want to use X¯ (sample mean) to understand μX.
#' Q
#' Which of the following uses CLT to understand how well X¯ approximates μX?
#' A
#' X¯ follows a normal distribution with mean μX and standard deviation σX / sqrt(12)
#' where σX is the population standard deviation.


# ex5
#' The result above tells us the distribution of the following random variable:
#' Z = sqrt(12) / (X¯ − μX) / σX.
#' Q
#' What does the CLT tell us is the mean of Z (you don't need code)?
#' A
#' 0


# ex6
#' The result of 4 and 5 tell us that we know the distribution of the difference
#' between our estimate and what we want to estimate, but don't know. However, the
#' equation involves the population standard deviation σX, which we don't know.
#' Q
#' Given what we discussed, what is your estimate of σX?
#' Hint: While the popsd() function from rafalib calculates population standard
#' deviations, the sd() function in base R calculates sample standard deviations.
#' A
sigma <- sd(X)


# ex7
#' Use the CLT to approximate the probability that our estimate X¯ (sample mean)
#' is off by more than 2 grams from μX (population mean).
#' A
2 * (1 - pnorm( 2 / sigma * sqrt(12) ))
# WHY????


# ex8
#' Q
#' What is the estimate of  SE(X¯−Y¯) = √(σ2Y / 12 + σ2X / 12)?
#' A
N <- length(X)
se <- sqrt(var(X)/N + var(Y)/N)
##or se <- sqrt(sd(X)^2/12 + sd(Y)^2/12)
se


# ex9
#' So now we can compute  Y¯−X¯ as well as an estimate of this standard error
#' and construct a t-statistic.
#' Q
#' What number is this t-statistic?
#' A
obs <- mean(X) - mean(Y)
tstats <- abs(obs) / se
##or t.test(Y,X)$stat
tstats
# Note: tstats == Z-value (see ex11)


# ex10
#' In probability and statistics, Student's t-distribution (or simply the
#' t-distribution) is any member of a family of continuous probability
#' distributions that arises when estimating the mean of a normally distributed
#' population in situations where the sample size is small and the population
#' standard deviation is unknown. The t-statistic, central limit theorem, is
#' telling us the null distribution is approximated by a normal distribution
#' with mean 0 and variance 1.
#' So the function pnorm tells us what proportion of normally distributed
#' data, with means 0 and standard deviation 1, are lower than whatever value
#' you put here.
#' the t-distribution is centered at 0 and has one parameter: the degrees of
#' freedom, that control the size of the tails. You will notice that if X follows
#' a t-distribution the probability that X is smaller than an extreme value such
#' as 3 standard deviations away from the mean grows with the degrees of freedom.
#' The t.test function calculates the degrees of freedom for you. One important
#' fact to keep in mind is that the degrees of freedom are directly related to
#' the sample size.
#' Q
#' If we apply the CLT, what is the distribution of this t-statistic?
#' A
#' Normal with mean 0 and standard deviation 1.


# ex11
#' Q
#' Now we are ready to compute a p-value using the CLT. What is the probability
#' of observing a quantity as large as what we computed in 9, when the null
#' distribution is true?
#' A
2 * (1 - pnorm(tstats))
#' or
#' Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
#' 2*( 1 - pnorm(Z) )


# ex12
#' CLT provides an approximation for cases in which the sample size is large.
#' In practice, we can't check the assumption because we only get to see 1
#' outcome (which you computed above). As a result, if this approximation is
#' off, so is our p-value. As described earlier, there is another approach that
#' does not require a large sample size, but rather that the distribution of the
#' population is approximately normal. We don't get to see this distribution so
#' it is again an assumption, although we can look at the distribution of the
#' sample with qqnorm(X) and qqnorm(Y). If we are willing to assume this, then
#' it follows that the t-statistic follows the t-distribution.
#' Q
#' What is the p-value under the t-distribution approximation?
#' Hint: use the t.test() function.
#' A
t.test(X, Y)$p.value


# ex13
#' With the CLT distribution, we obtained a p-value smaller than 0.05 (ex11) and
#' with the t-distribution, one that is larger (ex12). They can't both be right.
#' Q
#' What best describes the difference?
#' A
#' These are two different assumptions. The t-distribution accounts for the
#' variability introduced by the estimation of the standard error and thus,
#' under the null, large values are more probable under the null distribution.