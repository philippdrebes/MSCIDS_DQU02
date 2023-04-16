
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("dbscan")) install.packages("dbscan", dependencies=TRUE)

library(readxl)
library(dbscan)



# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

data <- read_excel("<YOUR PATH>/data_extreme.xlsx")
head(data)
plot(data)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Run algorithm

kNNdistplot(data,  k = 4)

dbscanResult <- dbscan(data, eps= 15, minPts=4)
dbscanResult

hullplot(data, dbscanResult)


dbscanResult <- dbscan(data, eps= 4, minPts=4)
dbscanResult

hullplot(data, dbscanResult)

