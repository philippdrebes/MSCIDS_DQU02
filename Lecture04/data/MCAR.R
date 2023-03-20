
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("naniar")) install.packages("naniar", dependencies=TRUE)
if (!require("mice")) install.packages("mice", dependencies=TRUE)

library(naniar)
library(mice)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Data set

head(airquality)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Missing pattern

md.pattern(airquality)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Little’s test

mcar_test(airquality)

