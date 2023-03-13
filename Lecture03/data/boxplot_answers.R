
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
library(readxl)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

task_02 <- read_excel("<YOUR PATH>/task_02.xlsx")
head(task_02)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Create boxplot

boxplot(task_02$Variable_A, ylim = c(-2, 10))
boxplot(task_02$Variable_B, ylim = c(-2, 10))
boxplot(task_02$Variable_C, ylim = c(-2, 10))


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Create histogram

hist(task_02$Variable_A)
hist(task_02$Variable_B)
hist(task_02$Variable_C)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Create table

F <- table(task_02$Variable_B)

f <- prop.table(F)
f <- round(f, digits = 2)

Fcum <- cumsum(F)
fcum <- cumsum(f)
fcum <- round(fcum, digits = 2)

cbind(F, f, fcum)

