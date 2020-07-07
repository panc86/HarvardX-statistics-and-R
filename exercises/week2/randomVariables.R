install.packages("downloader")
install.packages("dplyr")
library(downloader)
library(dplyr)

dir.create("data", showWarnings = FALSE)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

mice <- read.csv(filename)
controls <- filter(mice, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatments <- filter(mice, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(treatments) - mean(controls)
#' is this delta due to the different diet?
#' Is it just that we happened to get 12 mice that were heavier? 
#' That is where the concept of a random variable is going to help us.

# entire population
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
controlFilename <- file.path("data", basename(url))
if(!file.exists(controlFilename)) download(url,destfile=controlFilename)

population <- read.csv(controlFilename)
population <- unlist(population)

mean(sample(population, 12))
#' each sample will have different mean to controls and treatments
#' because samples of 12 mice are randomly chosen every time we call it
# can we see a difference of 3 by chance?


# ex1
#' Q
#' 
#' A
mean(population)


# ex2
#' Q
#' 
#' A
set.seed(1)
abs(mean(sample(population, 5)) - mean(population))


# ex3
#' Q
#' 
#' A
set.seed(5)
abs(mean(sample(population, 5)) - mean(population))
