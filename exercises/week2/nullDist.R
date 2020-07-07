install.packages("downloader")
install.packages("dplyr")
library(dplyr)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

mice <- read.csv(filename)
controls <- filter(mice, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatments <- filter(mice, Diet=="hf") %>% select(Bodyweight) %>% unlist
obs <- mean(treatments) - mean(controls)
#' is this delta due to the different diet?
#' Is it just that we happened to get 12 mice that were heavier? 
#' That is where the concept of a random variable is going to help us.
#' how statistical inference is used to support scientific statements
#' p-value case
# entire population
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
controlFilename <- file.path("data", basename(url))
if(!file.exists(controlFilename)) download(url,destfile=controlFilename)
population <- read.csv(controlFilename)
population <- unlist(population)
#' what happens when the null hypothesis is true?
#' That is, there's no high fat diet effect.
#' So we can just assign control mice to the treatment group because we assume
#' the treatment to be ineffective.
#' We sample them from the same control population and now I'm guaranteed to
#' have no high fat diet effect. It is like buying 12 new mice, pretending to
#' give them a high fat diet - like some kind of placebo that's just a chow -
#' following them along, weighing them again, and repeating the calculation of
#' the difference in mean.
nulls <- replicate(10000, {
  controlTest <- sample(population, 12)
  treatmentTest <- sample(population, 12)
  mean(controlTest) - mean(treatmentTest)
})
hist(nulls)
#' We can compute the probability to see values as big as 3 gr,
#' the observation, under the null hypothesis.
#' And that will help us provide scientific support based
#' on statistical inference for our finding.
pValue <- mean(abs(nulls) > obs)
#' The p-value is the probability that an outcome from the
#' null distribution is bigger than what we observed when
#' the null hypothesis is true.


# ex1
#' Set the seed at 1, then using a for-loop take a random
#' sample of 5 mice 1,000 times. Save these averages.
#' Q
#' What proportion of these 1,000 averages are more than
#' 1 gram away from the average of `population`?
#' A
set.seed(1)
nulls <- replicate(1000, {
  mean(sample(population, 5))
})
hist(nulls)
mean(abs(nulls - mean(population)) > 1)


# ex2 (iterations x10):
#' Q
#' 
#' A
set.seed(1)
nulls <- replicate(10000, {
  mean(sample(population, 5))
})
hist(nulls)
mean(abs(nulls - mean(population)) > 1)


# ex3 (sample size x10):
#' Q
#' 
#' A
set.seed(1)
nulls <- replicate(1000, {
  mean(sample(population, 50))
})
hist(nulls)
mean(abs(nulls - mean(population)) > 1)
