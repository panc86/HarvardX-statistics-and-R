install.packages("downloader")
install.packages("dplyr")
library(downloader)
library(dplyr)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)
data <- read.csv("msleep_ggplot2.csv")
class(data)


# ex1
#' Q
#' select primates
#' A
primates <- filter(data, order == "Primates") %>% select(sleep_total) %>% unlist
mu = mean(primates)


# ex2
#' Q
#' 
#' A
filter(data, order=="Primates") %>% summarize(mean( sleep_total))
# or mean_by_order <- data %>% group_by(order) %>% summarize(mean(sleep_total))
# or summarise(.data = group_by(data, order), mean(sleep_total))
