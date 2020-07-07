install.packages("downloader")
install.packages("rafalib")
install.packages("dplyr")
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

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# The population difference of mean birth weights is about 8.9 ounces.
# The standard deviations of the nonsmoking and smoking groups are
# about 17.4 and 18.1 ounces, respectively.

# As we did with the mouse weight data, this assessment interactively
# reviews inference concepts using simulations in R. We will treat the
# babies dataset as the full population and draw samples from it to 
# simulate individual experiments. We will then ask whether somebody
# who only received the random samples would be able to draw correct
# conclusions about the population.
# We are interested in testing whether the birth weights of babies born
# to non-smoking mothers are significantly different from the birth weights
# of babies born to smoking mothers.

# ex1
# We can explore the trade off of power and Type I error concretely using the babies data. Since we have the full population, we know what the true effect size is (about 8.93) and we can compute the power of the test for true difference between populations.

# Set the seed at 1 and take a random sample of N=5 measurements from each of the smoking and nonsmoking datasets. Use the t-test function to find the p-value. (Note that you already performed this calculation in the last assessment.)

# The p-value is larger than 0.05 so using the typical cut-off, we would not reject. This is a type II error.
# Q
# Which of the following is *not* a way to decrease this type of error?
    #1 Increase our chance of a type I error
    #2 Take a larger sample size
    #3 Find a population for which the null is not true
    #4 Use a higher α (alpha) level.
# A
# 3
set.seed(1)
N <- 5
smokersPop <- sample(bwt.smoke, N)
controlPop <- sample(bwt.nonsmoke, N)
paste("p-value:", t.test(smokersPop, controlPop)$p.value)


# ex2
# Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times.
# Q
# What proportion of the time do we reject at the 0.05 level?
# A
set.seed(1)
N <- 5
I <- 10000
reject <- function(N, alpha=0.05){
    smokersPop <- sample(bwt.smoke, N)
    controlPop <- sample(bwt.nonsmoke, N)
    pval <- t.test(smokersPop, controlPop)$p.value
    pval < alpha
}
rejections <- replicate(I, reject(N))
paste("rejection proportion:", mean(rejections))


# ex3
# Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for samples sizes of 30, 60, 90 and 120.
# Q
# Which of those four gives you power of about 80%?
# A
# 60
sizes <- seq(30, 120, 30)
power <- sapply(sizes, function(N){
    rejections <- replicate(I, reject(N))
    mean(rejections)
})
plot(sizes, power, type="b")


# ex4
# Repeat the problem above, but now require an  α  level of 0.01.
# Q
# Which of those four gives you power of about 80%?
# A
# 90
power <- sapply(sizes, function(N){
    rejections <- replicate(I, reject(N, alpha=0.01))
    mean(rejections)
})
plot(sizes, power, type="b")
