library(readxl)

hse <- read_excel('data/HSE.xlsx')
head(hse)
boxplot(hse$wtval ~ hse$sex)

#Missings-------------------------

if (!require("naniar")) install.packages("naniar", dependencies=TRUE)
if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("mice")) install.packages("mice", dependencies=TRUE)

library(naniar)
library(mice)

head(riskfactors)
missingPattern <- md.pattern(riskfactors)
missingPattern

mcarResult <- mcar_test(riskfactors)
mcarResult

if (mcarResult$p.value < 0.05) {
  print('Data is not missing completely at random')
} else {
  print('Data is MCAR.')
}

#Imputation-------------------------

grades <- read_excel('data/grades.xlsx')
head(grades)
md.pattern(grades)

grades.imp.mean <- mice(grades, method="mean", m=1, maxit=1)
grades.imp.sample <- mice(grades, method="sample", m=1, maxit=1)

grades.mean <- complete(grades.imp.mean)
grades.sample <- complete(grades.imp.sample)

cat('\n\nmean TakeHome before imputation: ', mean(grades$TakeHome, na.rm = TRUE))
cat('\nmean TakeHome after imputation [mean]: ', mean(grades.mean$TakeHome, na.rm = TRUE))
cat('\nmean TakeHome after imputation [sample]: ', mean(grades.sample$TakeHome, na.rm = TRUE))

cat('\n\nmean Final before imputation:', mean(grades$Final, na.rm = TRUE))
cat('\nmean Final after imputation [mean]:', mean(grades.mean$Final, na.rm = TRUE))
cat('\nmean Final after imputation [sample]:', mean(grades.sample$Final, na.rm = TRUE))

head(grades.mean, n = 10)
head(grades.sample, n = 10)

#Boxplot-------------------------

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("outliers")) install.packages("outliers", dependencies=TRUE)
if (!require("EnvStats")) install.packages("EnvStats", dependencies=TRUE)

library(outliers)
library(readxl)

hse <- read_excel('data/HSE.xlsx')
head(hse)

summary(hse)

min(hse$wtval, na.rm = TRUE)
max(hse$wtval, na.rm = TRUE)

hse_data <- na.omit(hse)
hist(hse_data$wtval, breaks = sqrt(nrow(hse_data)))

boxplot(hse_data$wtval)
out <- boxplot.stats(hse_data$wtval)$out
length(out)

out_index <- which(hse_data$wtval %in% c(out))
out_index

hse_data[out_index, ]

boxplot(hse_data$wtval)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#Percentile-------------------------

lower_bound <- quantile(hse_data$wtval, 0.025)
lower_bound

upper_bound <- quantile(hse_data$wtval, 0.975)
upper_bound

outlier_index <- which(hse_data$wtval < lower_bound | hse_data$wtval > upper_bound)
outlier_index

#GrubbsRosner-------------------------

library(readxl)
library(outliers)
library(EnvStats, quietly = T)

hse <- read_excel('data/HSE.xlsx')
hse_data <- na.omit(hse)
head(hse_data, 10)

grubbs.test(hse_data$wtval)
grubbs.test(hse_data$wtval, opposite = TRUE)

test.rosner <- rosnerTest(hse_data$wtval, k = 25)
test.rosner$all.stats

#K-means-------------------------

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("MVA")) install.packages("MVA", dependencies=TRUE)
if (!require("cluster")) install.packages("cluster", dependencies=TRUE)

library(readxl)
library(MVA)
library(cluster)

data <- read_excel("data/learning.xlsx")
any(is.na(data))
summary(data)
plot(data)

data.scaled <- scale(data, center = FALSE, scale = TRUE)
dists <- dist(data.scaled)
summary(data.scaled)
boxplot(data.scaled)
plot(data.scaled)


reps <- rep(0, 6)
for (i in 1:6) reps[i] <- sum(kmeans(data.scaled, centers = i, nstart = 20)$withinss)
plot(1:6, reps, type = "b", xlab = "Number of groups", ylab = "Sum of squares")


km.4 <- kmeans(data.scaled, centers = 4, nstart = 10)
km.4.groups <- km.4$cluster
km.4.groups

cluster.size.4 <- cbind(sum(km.4.groups == 1), sum(km.4.groups == 2), 
                        sum(km.4.groups == 3), sum(km.4.groups == 4))
cluster.size.4

plot(data.scaled, pch = km.4.groups, col=km.4.groups, lwd=2)
legend("topleft", legend = 1:4, pch = 1:4, col=1:4, bty="n")

plot(silhouette(km.4.groups, dists))


km.3 <- kmeans(data.scaled, centers = 3, nstart = 10)
km.3.groups <- km.3$cluster
km.3.groups

cluster.size.3 <- cbind(sum(km.3.groups == 1), sum(km.3.groups == 2), 
                        sum(km.3.groups == 3))
cluster.size.3

plot(data.scaled, pch = km.3.groups, col=km.3.groups, lwd=2)
legend("topleft", legend = 1:3, pch = 1:3, col=1:3, bty="n")

plot(silhouette(km.3.groups, dists))


dc <- dist(data.scaled, method = "euclidean")
dc

cc <- hclust(dc, method = "complete")
plot(cc,cex = 0.3, hang = -1)

#RANSAC-------------------------

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("dbscan")) install.packages("dbscan", dependencies=TRUE)

ransac <- function(data, n, k, t, d) {
  iterations <- 0
  bestfit <- NULL
  besterr <- 1e5
  while (iterations < k) {
    maybeinliers <- sample(nrow(data), n)
    maybemodel <- lm(y ~ x, data = data, subset = maybeinliers)
    alsoinliers <- NULL
    for (point in setdiff(1:nrow(data), maybeinliers)) {
      if (abs(maybemodel$coefficients[2]*data[point, 1] - data[point, 2] +
              maybemodel$coefficients[1])/(sqrt(maybemodel$coefficients[2] + 1)) < t)
        alsoinliers <- c(alsoinliers, point)
    }
    if (length(alsoinliers) > d) {
      bettermodel <- lm(y ~ x, data = data, subset = c(maybeinliers, alsoinliers))
      thiserr <- summary(bettermodel)$sigma
      if (thiserr < besterr) {
        bestfit <- bettermodel
        besterr <- thiserr
      }
    }
    iterations <- iterations + 1
  }
  bestfit
}

library(readxl)

learning <- read_xlsx('data/learning.xlsx')
head(learning)
plot(success ~ effort, data = learning)
abline(lm(success ~ effort, data = learning))

learning$x <- learning$effort 
learning$y <- learning$success
learning <- subset(learning, select = c(x, y))

set.seed(998899)
abline(ransac(learning, n = 10, k = 10, t = 0.5, d = 10), col = "blue")
legend(x = 'topleft', legend = c('simple lm', 'ransac'), col = c('black', 'blue'), lwd = 2)

#DBSCAN-------------------------

library(dbscan)

kNNdistplot(learning,  k = 4)
abline(h = 1.32, col = 'red', lwd = 1)

dbscanResult <- dbscan(learning, eps= 1.32, minPts=4)
dbscanResult

hullplot(learning, dbscanResult, xlab = 'effort', ylab = 'success')

#QQPlot-------------------------

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("DescTools")) install.packages("DescTools", dependencies=TRUE)

library(readxl)
library(DescTools)

data_hse <- read_excel("hse2016_eul_small.xlsx")
head(data_hse, 10)

qqnorm(data_hse$Age35g)
qqline(data_hse$Age35g)
hist(data_hse$Age35g)

qqnorm(data_hse$srcin01d)
qqline(data_hse$srcin01d)
hist(data_hse$srcin01d)

qqnorm(data_hse$Height)
qqline(data_hse$Height)
hist(data_hse$Height)

qqnorm(data_hse$Weight)
qqline(data_hse$Weight)
hist(data_hse$Weight)

qqnorm(data_hse$BMI)
qqline(data_hse$BMI)
hist(data_hse$BMI)

#QQPlot-Sim-------------------------

numbers <- c(1000, 10000, 100000, 1000000)

for (i in numbers) {
  x <- rnorm(n = i)
  
  start_time <- Sys.time()
  qqnorm(x)
  qqline(x)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  sprintf('QQ-PLot for %i data points\n', i)
}

#Misc-------------------------

##Cluster------------

# K-MEANS CLUSTERING ANALYSIS CODE

# load the necessary libraries
library(readxl)
library(MVA)
library(cluster)

# Load the dataset , change the name of the xlsx file and uncomment out the code to load the dataset!
library(readxl) # to load the xlsx dataset!

# check first 5 rows of dataset
head(data)

# plot the dataset to get an overview of the clusters 
plot(data)
## INTERPRETATION OF THE ORIGINAL UNCLUSTERED SCATTER PLOT:


# normalise the colums of the dataset for k means cluster preprocessing
datas <- scale(data, center = FALSE, scale = TRUE)
# plot a boxplot of the normalised dataset
boxplot(datas)
# plot a scatterplot of the normalised dateset
plot(datas)

# k-means Preparation: To perform k-means clustering, distances between the data points are calculated.
dists <- dist(datas)

# pre-set k-means cluster-size to 4
km <- kmeans(datas, centers = 4, nstart = 10)
groups_km <- km$cluster
groups_km


# get the cluster values/numbers for each of the 4 clusters you pre-selected
cluster_size <- cbind(sum(groups_km == 1), sum(groups_km == 2), sum(groups_km == 3), sum(groups_km == 4))
cluster_size

# display the 4 clustered groups that you defined earlier in a scatterplot
plot(datas, pch = groups_km, col=groups_km, lwd=2)
legend("topright", legend = 1:4, pch = 1:4, col=1:4, bty="n")
## INTERPRETATION OF THE CLUSTERED SCATTER PLOT:

# Scree-plot code prep
reps <- rep(0, 6)
for (i in 1:6) reps[i] <- sum(kmeans(datas, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
# Scree Plot picture output
plot(1:6, reps, type = "b", xlab = "Number of groups", ylab = "Sum of squares")
# INTERPRETATION OF SCREE PLOT:




# silhoutte plot picture output
plot(silhouette(groups_km, dists))
# INTERPRETATION OF SILHOUETTE PLOT:






# dendogram plot pre-preparation
dc <- dist(datas, method = "euclidean")
dc
cc <- hclust(dc, method = "complete")
# plot of dendogram picture output
plot(cc,cex = 0.3, hang = -1)
# INTERPRETATION OF DENDOGRAM:




###############################################################################

###############################################################################
# TEST EXAMPLE OF HOW TO RUN THE CODE :


#libraries
library(readxl)
library(MVA)
library(cluster)

# Load the dataset , change the name of the xlsx file and uncomment out the code to load the dataset!
library(readxl) # to load the xlsx dataset!
data <- read_excel("learning.xlsx")

# check first 5 rows of dataset
head(data)

# plot the dataset to get an overview of the clusters 
plot(data)

# normalise the colums of the dataset for k means cluster preprocessing
datas <- scale(data, center = FALSE, scale = TRUE)
# plot a boxplot of the normalised dataset
boxplot(datas)
# plot a scatterplot of the normalised dateset
plot(datas)

# k-means Preparation: To perform k-means clustering, distances between the data points are calculated.
dists <- dist(datas)

# pre-set k-means cluster-size to 4
km <- kmeans(datas, centers = 4, nstart = 10)
groups_km <- km$cluster
groups_km


# get the cluster values/numbers for each of the 4 clusters you pre-selected
cluster_size <- cbind(sum(groups_km == 1), sum(groups_km == 2), sum(groups_km == 3), sum(groups_km == 4))
cluster_size

# display the 4 clustered groups that you defined earlier in a scatterplot
plot(datas, pch = groups_km, col=groups_km, lwd=2)
legend("topright", legend = 1:4, pch = 1:4, col=1:4, bty="n")


# Scree-plot code prep
reps <- rep(0, 6)
for (i in 1:6) reps[i] <- sum(kmeans(datas, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
# Scree Plot picture output
plot(1:6, reps, type = "b", xlab = "Number of groups", ylab = "Sum of squares")

# INTERPRETATION OF Scree plot:



# silhoutte plot picture output
plot(silhouette(groups_km, dists))

# INTERPRETATION OF Silhouette plot:


# dendogram plot pre-preparation
dc <- dist(datas, method = "euclidean")
dc

cc <- hclust(dc, method = "complete")

# plot of dendogram picture output
plot(cc,cex = 0.3, hang = -1)


# INTERPRETATION OF DENDOGRAM:



##DBSCAN------------
# load the necessary libraries
library(readxl)
library(dbscan)

#load the correct data file called : data.xlsx
#data <- read_excel("data.xlsx").  # CHANGE ACCORDINGLY!!
# check first 5 rows of dataset
head(data)
# plot a scatterplot of the dataset
plot(data)

# plot the knn distpot, read from the y axis or the vertical axis to change the eps setting accordingly!
library(dbscan)
# plot of kNNdistplot diagram
kNNdistplot(data,  k = 4)

# change the eps setting in the dbsscan function to the correct value according to the kNNdisplot, check for outliers!
dbscanResult <- dbscan(data, eps= 1.3, minPts=4)
# Check and interpret the dbscanResult!
dbscanResult

# plot the hullplot of the dbscanResult
hullplot(data, dbscanResult)
# INTERPRET the hullplot dbsscan results!!!!!!




# PLAY AROUND WITH THE eps setting!!!!!!!!!
# other dbs scan  eps settings...
# when eps = 15
dbscanResult <- dbscan(data, eps= 15, minPts=4)
dbscanResult
hullplot(data, dbscanResult)

# when eps = 4
dbscanResult <- dbscan(data, eps= 4, minPts=4)
dbscanResult
hullplot(data, dbscanResult)

###############################################################################

###############################################################################

# EXAMPLE OF HOW TO RUN DBSCAN CODE!!!

# load the necessary libraries
library(readxl)
library(dbscan)

#load the correct data file called : data.xlsx
data <- read_excel("learning.xlsx")
# check first 5 rows of dataset
head(data)
# plot a scatterplot of the dataset
plot(data)

# plot the knn distpot, read from the y axis or the vertical axis to change the eps setting accordingly!
library(dbscan)
# plot of kNNdistplot diagram
kNNdistplot(data,  k = 4)

# change the eps setting in the dbsscan function to the correct value according to the kNNdisplot, check for outliers!
dbscanResult <- dbscan(data, eps= 1.3, minPts=4)
# Check and interpret the dbscanResult!
dbscanResult

# plot the hullplot of the dbscanResult
hullplot(data, dbscanResult)
# INTERPRET the hullplot dbsscan results!!!!!!




# PLAY AROUND WITH THE eps setting!!!!!!!!!
# other dbs scan  eps settings...
# when eps = 15
dbscanResult <- dbscan(data, eps= 15, minPts=4)
dbscanResult
hullplot(data, dbscanResult)

# when eps = 4
dbscanResult <- dbscan(data, eps= 4, minPts=4)
dbscanResult
hullplot(data, dbscanResult)






##Missings-------------------

# MISSINGNESS CODE: 

# Load the necessary libraries
library(naniar)  # to use mcar_test() function
library(mice)   # to use md.pattern() function
library(readxl) # to load the xlsx dataset!

# Load the dataset , change the name of the xlsx file and uncomment out the code to load the dataset!
library(readxl) # to load the xlsx dataset!

# check first 5 rows of this dataset
head(data)
colnames(data)

# CHECK THE TOTAL NUMBER OF MISSING VALUES IN THE DATASET
sum(is.na(data))

# Calculate the total number of data points
total_datapoints <- nrow(data) * ncol(data)

# Print the result
print(total_datapoints)
#INTERPRETATION: The total number of datapoints are: type the value here...


# md.pattern() function comes from the mice package!!!
library(mice)
md.pattern(data)

# INTERPRETATION of md.pattern of the dataset.




# mcartest() function is part of naniar package!!!
library(naniar)
mcar_test(data)

# INTERPRETATION of the mcar_test of the dataset.




# check the means of the column1 and column2, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!:
mean(data$column1, na.rm = TRUE)
mean(data$column2, na.rm = TRUE)
# INTERPRETATION...do the means of these column vary greatly, if comparing 2 columns in the dataset?!





# imputing the missing values in the data using the mean method (method="mean"):

# impute the missing values of the original dataset with the mean value
imp_data_mean <- mice(data, method="mean", m=1, maxit=1)

# create a new dataframe with the missing values imputed with the mean value and name it data_mean which is a new dataset that has the mising values of each column replaced with the mean value of each column!
data_mean <- complete(imp_data_mean)

# check first 10 rows of the original dataset with the missing values
head(data,10)
# check first 10 rows of the imputed dataset with the missing values replaced with the mean values of each column
head(data_mean,10)

# check the means of column1 of original dataset and imputed dataset, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!
mean(data$column1, na.rm = TRUE)
mean(data_mean$column1, na.rm = TRUE)
# INTERPRETATION: Is the means of the original and imputed dataset the same??!!





# check the means of column2 of original dataset and imputed dataset, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!
mean(data$column2, na.rm = TRUE)
mean(data_mean$column2, na.rm = TRUE)
# INTERPRETATION: Is the means of the original and imputed dataset the same??!!






##################################################################################################################################

##################################################################################################################################
### EXAMPLE OF HOW TO RUN THE CODE!!!!

# MISSINGNESS CODE: 

# Load the necessary libraries
library(naniar)  # to use mcar_test() function
library(mice)   # to use md.pattern() function
library(readxl) # to load the xlsx dataset!

# Load the dataset , change the name of the xlsx file and uncomment out the code to load the dataset!
library(readxl) # to load the xlsx dataset!
data <- read_excel("Task_01.xlsx")

# check first 5 rows of this dataset
head(data)
colnames(data)

# CHECK THE TOTAL NUMBER OF MISSING VALUES IN THE DATASET
sum(is.na(data))

# Calculate the total number of data points
total_datapoints <- nrow(data) * ncol(data)


# Print the result
print(total_datapoints)

#INTERPRETATION: The total number of datapoints are: type the value here...


# md.pattern() function comes from the mice package!!!
library(mice)
md.pattern(data)

# INTERPRETATION of md.pattern of the dataset.




# mcartest() function is part of naniar package!!!
library(naniar)
mcar_test(data)

# INTERPRETATION of the mcar_test of the dataset.




# check the means of the column1 and column2, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!:
mean(data$var_D, na.rm = TRUE)
mean(data$var_E, na.rm = TRUE)

# INTERPRETATION...do the means of these column vary greatly, if comparing 2 columns in the dataset?!





# imputing the missing values in the data using the mean method (method="mean"):

# impute the missing values of the original dataset with the mean value
imp_data_mean <- mice(data, method="mean", m=1, maxit=1)

# create a new dataframe with the missing values imputed with the mean value and name it data_mean which is a new dataset that has the mising values of each column replaced with the mean value of each column!
data_mean <- complete(imp_data_mean)

# check first 10 rows of the original dataset with the missing values
head(data,10)
# check first 10 rows of the imputed dataset with the missing values replaced with the mean values of each column
head(data_mean,10)

# check the means of column1 of original dataset and imputed dataset, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!
mean(data$var_D, na.rm = TRUE)
mean(data_mean$var_D, na.rm = TRUE)
# INTERPRETATION: Is the means of the original and imputed dataset the same??!!


# check the means of column2 of original dataset and imputed dataset, PLEASE CHANGE THE COLUMN NAMES!!!! code needs to be modified!!!
mean(data$var_E, na.rm = TRUE)
mean(data_mean$var_E, na.rm = TRUE)
# INTERPRETATION: Is the means of the original and imputed dataset the same??!!



