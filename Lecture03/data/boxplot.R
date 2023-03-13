
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)

library(readxl)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

example <- read_excel("<YOUR PATH>/example.xlsx")
head(example)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Create box plot

boxplot(example$salary ~ example$experience)

boxplot(example$salary ~ example$experience, ylim=c(30, 50), col= rgb(0.89,0.92,0.97))

