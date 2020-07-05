install.packages("downloader")
install.packages("dplyr")
library(downloader)
library(dplyr)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- basename(url)
download(url,filename)
data <- read.csv("femaleMiceWeights.csv")

#' Q
#' 
#' A
controls <- filter(data, Diet=="chow") %>% select(Bodyweight) %>% unlist
