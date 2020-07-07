library(dplyr)
library(rafalib)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

babies <- read.table(filename, header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

#' we can look for the true population difference in means between smoking
#' and non-smoking birth weights
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


# ex1
#' Set the seed at 1 and obtain a samples from the non-smoking mothers (dat.ns)
#' of size  N=25 . Then, without resetting the seed, take a sample of the same
#' size from and smoking mothers (dat.s). Compute the t-statistic (call it tval).
#' Q
#' What is the absolute value of the t-statistic?
#' A
set.seed(1)
N <- 25
nonsmoker <- sample(bwt.nonsmoke, N)
smoker <- sample(bwt.smoke, N)
tval <- abs(mean(nonsmoker) - mean(smoker)) / sqrt(var(nonsmoker)/N + var(smoker)/N)
tval
#' The t-value measures the size of the difference relative to the variation in 
#' your sample data. Put another way, T is simply the calculated difference 
#' represented in units of standard error. The greater the magnitude of T, the 
#' greater the evidence against the null hypothesis. This means there is greater 
#' evidence that there is a significant difference. The closer T is to 0, the 
#' more likely there isn't a significant difference.
#' https://blog.minitab.com/blog/statistics-and-quality-data-analysis/what-are-t-values-and-p-values-in-statistics#:~:text=The%20t%2Dvalue%20measures%20the,evidence%20against%20the%20null%20hypothesis.


# ex2
#' Recall that we summarize our data using a t-statistics because we know that 
#' in situations where the null hypothesis is true (what we mean when we say 
#' "under the null") and the sample size is relatively large, this t-value will 
#' have an approximate standard normal distribution. Because we know the 
#' distribution of the t-value under the null, we can quantitatively determine 
#' how unusual the observed t-value would be if the null hypothesis were true.
#' The standard procedure is to examine the probability a t-statistic that 
#' actually does follow the null hypothesis would have larger absolute value 
#' than the absolute value of the t-value we just observed -- this is called a 
#' two-sided test. We have computed these by taking one minus the area under 
#' the standard normal curve between -abs(tval) and abs(tval). In R, we can do 
#' this by using the pnorm() function, which computes the area under a normal 
#' curve from negative infinity up to the value given as its first argument:
#' `pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))`
#' Q
#' What is this estimated p-value?
#' A
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval)))


# ex3
#' Because of the symmetry of the standard normal distribution, there is a
#' simpler way to calculate the probability that a t-value under the null could 
#' have a larger absolute value than tval.
#' Q
#' Choose the simplified calculation from the following:
#' A
#' `2*pnorm(-abs(tval))`
#' By reporting only p-values, many scientific publications provide an incomplete 
#' story of their findings. As we have mentioned, with very large sample sizes, 
#' scientifically insignificant differences between two groups can lead to small 
#' p-values. Confidence intervals are more informative as they include the 
#' estimate itself. We will learn how to compute these in the next module.