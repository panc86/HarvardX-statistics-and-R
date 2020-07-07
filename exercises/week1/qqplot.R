load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  y <- dat[,i]
  qqnorm(y, main=paste(c("Feat #", i), sep=""))
}
#' Examine each of these two columns using a histogram.
#' Note which column has "positive skew", in other words the
#' histogram shows a long tail to the right (toward larger values).
#' Note which column has "negative skew", that is, a long tail to
#' the left (toward smaller values). Note that positive skew looks
#' like an up-shaping curve in a qqnorm() plot, while negative skew
#' looks like a down-shaping curve.
#' Q
#' 
#' A
par(mfrow=c(2,1))
hist(dat[,4])
hist(dat[,9])
