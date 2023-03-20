
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("naniar")) install.packages("naniar", dependencies=TRUE)
if (!require("mice")) install.packages("mice", dependencies=TRUE)

library(naniar)
library(mice)

options(max.print = 2000)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Data set

head(riskfactors)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Missing pattern

md.pattern(riskfactors)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Little’s test

mcar_test(riskfactors)

