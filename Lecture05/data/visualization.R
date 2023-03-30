
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("outliers")) install.packages("outliers", dependencies=TRUE)

library(readxl)
library(outliers)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

mpg <- read_excel("<YOUR PATH>/mpg.xlsx")
head(mpg,10)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Key figures & Graphs

summary(mpg$hwy)
min(mpg$hwy)
max(mpg$hwy)

hist(mpg$hwy,breaks = sqrt(nrow(mpg)))


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Box plot

boxplot(mpg$hwy)
boxplot(mpg$hwy)$out

out <- boxplot.stats(mpg$hwy)$out
out_ind <- which(mpg$hwy %in% c(out))
out_ind

mpg[out_ind, ]

boxplot(mpg$hwy)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Percentiles method

lower_bound <- quantile(mpg$hwy, 0.025, na.rm = TRUE)
lower_bound

upper_bound <- quantile(mpg$hwy, 0.975, na.rm = TRUE)
upper_bound

outlier_ind <- which(mpg$hwy < lower_bound | mpg$hwy > upper_bound)
outlier_ind

