
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("outliers")) install.packages("outliers", dependencies=TRUE)
if (!require("EnvStats")) install.packages("EnvStats", dependencies=TRUE)

library(readxl)
library(outliers)
library(EnvStats)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

mpg <- read_excel("<YOUR PATH>/mpg.xlsx")
head(mpg,10)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Grubbs

test <- grubbs.test(mpg$hwy)
test

test <- grubbs.test(mpg$hwy, opposite = TRUE)
test


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Rosner

test <- rosnerTest(mpg$hwy, k = 3)
test

test$all.stats


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Mahalanobis

air = airquality[c("Ozone", "Temp")]
air = na.omit(air)
air.center = colMeans(air)
air.cov = cov(air)

rad = qchisq(p = 0.95 , df = ncol(air))
rad = sqrt(rad)

plot(air$Temp, air$Ozone)

distances <- mahalanobis(x = air , center = air.center , cov = air.cov)
cutoff <- qchisq(p = 0.95 , df = ncol(air))

air[distances > cutoff ,]

