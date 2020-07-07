install.packages("downloader")
install.packages("rafalib")
install.packages("dplyr")
library(dplyr)
library(rafalib)
library(downloader)
dir.create("data", showWarnings = FALSE)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- file.path("data", basename(url))
if(!file.exists(filename)) download(url,destfile=filename)

dat <- read.csv(filename) 
dat <- na.omit( dat )
head(dat)


# ex1
#' Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
#' Q
#' What is this population's average?
#' A
x <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(x)


# ex2
#' Now use the rafalib package and use the popsd() function to compute
#' the population standard deviation.
mxstd <- popsd(x)


# ex3
#' Set the seed at 1. Take a random sample  X  of size 25 from x.
#' Q
#' What is the sample average?
#' A
set.seed(1)
X <- sample(x, 25)
mean(X)


# ex4
#' Use dplyr to create a vector y with the body weight of all males on
#' the high fat hf) diet.
#' Q
#' What is this population's average?
#' A
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(y)


# ex5
#' Now use the rafalib package and use the popsd() function to compute
#' the population standard deviation.
mystd <- popsd(y)


# ex6
#' Set the seed at 1. Take a random sample  Y  of size 25 from y.
#' Q
#' What is the sample average?
#' A
set.seed(1)
Y <- sample(y, 25)
mean(Y)


# ex7
#' Q
#' What is the difference in absolute value between  y¯−x¯  and  Y¯−X¯?
#' A
abs((mean(y)-mean(x)) - (mean(Y)-mean(X)))


# ex8
#' Repeat the above for females, this time setting the seed to 2.
#' Q
#' What is the difference in absolute value between  y¯−x¯  and  Y¯−X¯ ?
#' Make sure to set the seed to 2 before each sample() call.
#' This function should be called twice.
x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
fxstd <- popsd(x)
set.seed(2)
X <- sample(x, 25)
mean(X)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
fystd <- popsd(y)
set.seed(2)
Y <- sample(y, 25)
mean(Y)
#' A
abs((mean(y)-mean(x)) - (mean(Y)-mean(X)))


# ex9
#' For the females, our sample estimates were closer to the population
#' difference than with males.
#' Q
#' What is a possible explanation for this?
#' A
mxstd > fxstd
mystd > fystd
#' The population variance of the females is smaller than that of the males;
#' thus, the sample variable has less variability.