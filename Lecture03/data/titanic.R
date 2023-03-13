
# Install packages --------------------------------------------------------

if (!require("vcd")) install.packages("vcd", dependencies=TRUE)

library(vcd)


# Read data ---------------------------------------------------------------

dfTitanic <- as.data.frame(Titanic)
head(dfTitanic)



# Answer questions --------------------------------------------------------

# Is there a significant relation between survival and class?

structable(~ Class + Survived, data = dfTitanic)
mosaic(~ Class + Survived, data = dfTitanic, shade = TRUE)


# Compare the survival of men and women.

mosaic(~ Sex + Survived, data = dfTitanic, shade = TRUE)


# How do you assess this question: Does the "women-first policy" apply in all classes?
x11()
cotabplot(~ Survived + Sex | Class, data = dfTitanic, shade = TRUE)
