
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)

library(readxl)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Define function

ransac <- function(data, n, k, t, d) {
  iterations <- 0
  bestfit <- NULL
  besterr <- 1e5
  while (iterations < k) {
    maybeinliers <- sample(nrow(data), n)
    maybemodel <- lm(y ~ x, data = data, subset = maybeinliers)
    alsoinliers <- NULL
    for (point in setdiff(1:nrow(data), maybeinliers)) {
      if (abs(maybemodel$coefficients[2]*data[point, 1] - data[point, 2] + maybemodel$coefficients[1])/(sqrt(maybemodel$coefficients[2] + 1)) < t)
        alsoinliers <- c(alsoinliers, point)
    }
    if (length(alsoinliers) > d) {
      bettermodel <- lm(y ~ x, data = data, subset = c(maybeinliers, alsoinliers))
      thiserr <- summary(bettermodel)$sigma
      if (thiserr < besterr) {
        bestfit <- bettermodel
        besterr <- thiserr
      }
    }
    iterations <- iterations + 1
  }
  bestfit
}


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data & Run algorithm

data <- read_excel("<YOUR PATH>/RANSAC.xlsx")
head(data)
plot(data)

abline(lm(y ~ x, data = data))
set.seed(1234)
abline(ransac(data, n = 10, k = 10, t = 0.5, d = 10), col = "blue")


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data & Run algorithm

data <- read_excel("<YOUR PATH>/Fischler_Bolles.xlsx")
head(data)
plot(data, ylim=c(0,5))

abline(lm(y ~ x , data = data))
set.seed(123)
abline(ransac(data, n = 3, k = 10, t = 0.5, d = 2), col = "blue")

