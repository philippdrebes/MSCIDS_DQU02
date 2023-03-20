
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("mice")) install.packages("mice", dependencies=TRUE)

library(readxl)
library(mice)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

grades <- read_excel("<YOUR PATH>/grades.xlsx")
grades


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Missing pattern & Key figures

md.pattern(grades)

mean(grades$TakeHome, na.rm = TRUE)
mean(grades$Final, na.rm = TRUE)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Imputation

imp_grades_mean <- mice(grades, method="mean", m=1, maxit=1)
imp_grades_mean <- mice(grades, method="sample", m=1, maxit=1)

grades_mean <- complete(imp_grades_mean)

head(grades,10)
head(grades_mean,10)

mean(grades$TakeHome, na.rm = TRUE)
mean(grades_mean$TakeHome, na.rm = TRUE)

mean(grades$Final, na.rm = TRUE)
mean(grades_mean$Final, na.rm = TRUE)

