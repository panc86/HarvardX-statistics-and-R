library(downloader)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

x <- unlist( read.csv(filename) )
# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}
# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

# ex1
#' Q
#' Use a histogram to "look" at the distribution of averages we get with a
#' sample size of 5 and a sample size of 50. How would you say they differ?
par(mfrow=c(1, 2))
limits <- c(range(x))
hist(averages5, main="With 5 random samples", xlim=limits)
hist(averages50, main="With 50 random samples", xlim=limits)
#' A
#' They both look roughly normal, but with a sample size of 50
#' the spread is smaller.


# ex2
#' Q
#' For the last set of averages, the ones obtained from a sample size
#' of 50, what proportion are between 23 and 25?
#' A
mean(averages50 <= 25) - mean(averages50 <= 23)


# ex3
#' Q
#' What is the proportion of observations between 23 and 25 in a normal
#' distribution with average 23.9 and standard deviation 0.43?
#' Hint: Use pnorm() twice.
#' A
mu <- 23.9
sigma <- 0.43
pnorm(25, mu, sigma) - pnorm(23, mu, sigma)
#' Note that you can use the function pnorm() to find the proportion of
#' observations below a cutoff x given a normal distribution with mean mu
#' and standard deviation sigma with pnorm(x, mu, sigma) or pnorm((x-mu)/sigma)
