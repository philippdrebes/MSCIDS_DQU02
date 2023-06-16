
# Install packages --------------------------------------------------------

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("DescTools")) install.packages("DescTools", dependencies=TRUE)


library(readxl)
library(DescTools)



### FIRST PART: Replication of slide 15
## Test against normal distribution
## Simulation taking into account beta distribution → en.wikipedia.org/wiki/Beta_distribution

# Prepare data ------------------------------------------------------------

x <- runif(n  =  1000)

curve(dnorm(x),xlim=c(-3,3))
curve(dbeta(x,2,5),xlim=c(0,1))


# QQ plot -----------------------------------------------------------------

PlotQQ(rnorm(x))
PlotQQ(rbeta(x,2,5))



### SECOND PART: Replication of slide 16
## Test for outliers

# Prepare data ------------------------------------------------------------

norm_out <- read_excel("<YOUR PATH>/norm_out.xlsx")
head(norm_out,10)

x <- rnorm(n  =  1000)

hist(norm_out$norm_out, xlim = c(-6,6), breaks = 100)
hist(x, xlim = c(-6,6), breaks = 100)


# QQ plot -----------------------------------------------------------------

PlotQQ(norm_out$norm_out)
PlotQQ(x)



### THIRD PART: Exercise
## Examine variables from Health Survey for England 

# Prepare data ------------------------------------------------------------

data_hse <- read_excel("<YOUR PATH>/hse2016_eul.xlsx")
head(data_hse,10)

# For each variable VARIABLE run ...
qqnorm(data_hse$VARIABLE)
qqline(data_hse$VARIABLE)
hist(data_hse$VARIABLE)

