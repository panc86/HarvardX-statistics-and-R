install.packages("UsingR")
library(UsingR)
x <- father.son$fheight
round(sample(x, 20), 1)

# histogram
#' Q
#' 
#' A
hist(x, breaks=seq(floor(min(x)), ceiling(max(x))),
     main="Heights hist", xlab="Heights in inches")

