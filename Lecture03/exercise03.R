library(readxl)

# Task 1

hse <- read_excel('data/HSE.xlsx')

head(hse)
boxplot(hse$wtval ~ hse$sex)
