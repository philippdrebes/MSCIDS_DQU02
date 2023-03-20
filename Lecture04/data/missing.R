
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("mice")) install.packages("mice", dependencies=TRUE)

library(readxl)
library(mice)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

HSE <- read_excel("<YOUR PATH>/HSE_part.xlsx")
head(HSE,10)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Handling missing

mean(HSE$wtval)
mean(HSE$wtval, na.rm = TRUE)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Missing pattern

md.pattern(HSE)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Imputation

imp_HSE_mean <- mice(HSE, method="mean", m=1, maxit=10)

HSE_mean <- complete(imp_HSE_mean)

head(HSE,10)
head(HSE_mean,10)

mean(HSE_mean$wtval)

