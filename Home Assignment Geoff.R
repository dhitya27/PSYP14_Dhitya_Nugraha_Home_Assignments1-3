# Home Assignment 1 and 2
# Name: Dhitya Prasetya Dian Nugraha
# Course: Advanced Scientific Method in Psychology
# Teacher: Geoff Patching

##### Home Assignment Part 1 #####
womenshealth_dataset <- read.delim("~/Academic Stuff/Sweden/Academic/Autumn Course 2020/First Semester/Advanced Scientific Methods in Psychology/Assignments/Assignment 1/Home Assignment Geoff/womenshealth.txt")
View(womenshealth_dataset)

##### Removing NA cases #####
womenshealth_dataset_No_NA <- na.omit(womenshealth_dataset)
view(womenshealth_dataset_No_NA)

##### Essential R Packages for home assignment #####

install.packages("tidyverse")
library(tidyverse)

install.packages("psych")
library(psych)

install.packages("MVA")
library(MVA)

install.packages("MASS")
library(MASS)

install.packages("smacof")
library(smacof)

##### Checking the descriptive statistics from the dataset #####
summary(womenshealth_dataset)
describe(womenshealth_dataset)
head(womenshealth_dataset)

##### Part 1: Using R function to calculate covariance and correlation matrices from the Women's Health Text #####

##### Computing the covariance the easy way #####
cov(womenshealth_dataset_No_NA[, c("subno", "timedrs", "attdrug",
                             "atthouse", "income", "emplmnt",
                             "mstatus", "race", "log10.ltimedrs")])

##### Computing the correlation the easy way #####
cor(womenshealth_dataset_No_NA[, c("subno", "timedrs", "attdrug",
                                   "atthouse", "income", "emplmnt",
                                   "mstatus", "race", "log10.ltimedrs")])

##### Description of Women Health Dataset #####
class(womenshealth_dataset_No_NA) # Identifying the type of data
dim(womenshealth_dataset_No_NA) # Checking the number of rows and columns
head(womenshealth_dataset_No_NA) # Checking the first 6 rows of the data
str(womenshealth_dataset_No_NA) # Checking the structure for each variables

X <- womenshealth_dataset_No_NA[, c("subno", "timedrs", "attdrug",
                                    "atthouse", "income", "emplmnt",
                                    "mstatus", "race", "log10.ltimedrs")] # Assigning all of the variables into a new object "X"

##### Assigning X as the object that contains the data matrix (Items and Variables) #####
X <- as.matrix(womenshealth_dataset_No_NA) # Assigning value X to become object as data matrix 
class(X) # The type of data in this dataset has been changed to Matrix

##### Computing the variable mean in each columns #####
colMeans(X)

##### Creating the Covariance Matrices Step by step approach #####
n <- nrow(X) # Number of rows from the dataset
n
CovM <- diag(n) - matrix(1/n, n, n) # Creating the diagonal matrix 
Xcov <- CovM %*% X # Calculating the diagonal matrix with the X values that contains all of the variables using the matrix multiplication
Xs <- t(Xcov) %*% Xcov/(n-1) # Calculating the covariance matrix using the transpose function
Xs[1:9, 1:9]

cov(X) # The final output is the same

##### Second approach to calculate the Covariance and Correlation Matrices #####

Xcov <- scale(X, center = TRUE, scale = FALSE)
Scov <- t(Xcov) %*% Xcov / (n-1)
Scov[1:9, 1:9]

cov(X) # Final output is the same

##### Creating the Correlation Matrices Step by Step approach #####
n <- nrow(X)
n
CorM <- diag(n) - matrix(1/n, n, n) # Creating the diagonal matrix
Dcor <- diag(apply(X, 2, sd))
Xr <- CorM %*% X %*% solve(Dcor)
R_Matrix <- t(Xr) %*% Xr/(n-1) # Calculating the correlation matrix
R_Matrix[1:9, 1:9]

cor(X) # The final output is the same

##### Second approach to calculate correlation matrix #####

Xcor <- scale(X, center = TRUE, scale = TRUE)
r <- t(Xr) %*% Xr / (n-1)
r[1:9, 1:9]

cor(X) # Final output is the same

##### Home Assignment Part 2 Multidimensional Scaling #####
Nations <- read.delim("~/Academic Stuff/Sweden/Academic/Autumn Course 2020/First Semester/Advanced Scientific Methods in Psychology/Assignments/Assignment 1/Home Assignment Geoff/Nations.txt") # Original Data File

View(Nations) # View the data in a new window

str(Nations) # Checking the structure of the data

##### Using NMDS to examine students perceived dissimilarities between Nations #####   
Nations_NMDS_diss <- sim2diss(Nations, method = 7, to.dist = TRUE) # Converting the Nations dataset from dissimilarities to similarities

##### Calculating the Stress Values #####
Nations_mds <- isoMDS(Nations_NMDS_diss) # The stress value measures the difference between the observed similarities. In this case different nations and student perceived dissimilarities
Nations_mds$stress # Stress value = 19.034 (It's a bad fit)

##### Creating the Plots for NMDS #####
x <- Nations_mds$points[,1] # Assigning the X values
y <- Nations_mds$points[,2] # Assigning the Y values

plot(x, y, xlab = "Nations Ideology", ylab = "Economic Development",
     xlim = range(Nations_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(Nations), cex = 0.9)
abline(h = 0, v = 0, col = "gray60", lty = 2)

##### Creating the Nations Shephard Plot #####
Nations_Shepard <- Shepard(Nations_NMDS_diss, Nations_mds$points)
Nations_Shepard # The shephard plot helps us to see the distance comparison before and after

plot(Nations_Shepard, pch = 20, xlab = "Dissimilarity", ylab = "Distance",
     xlim = range(Nations_Shepard$x),
     ylim = range(Nations_Shepard$x))
lines(Nations_Shepard$x, Nations_Shepard$yf, type = "S")
Nations_sh <- Shepard(Nations[lower.tri(Nations)], Nations_mds$points)
Nations_sh

##### This not part of the home assignment but I want to see the output of MDS using Euclidean Distance #####

##### Using Multidimensional Scaling (MDS) to examine students perceived dissimilarities between Nations #####
distance.matrix <- dist(scale(t(Nations), center = TRUE, scale = TRUE), method = "euclidean") # Creating the Distance Matrix using "Euclidean" Method
mds.Nations <- cmdscale(distance.matrix, eig = TRUE, x.ret = TRUE) # Perform the MDS on the Distance Matrix using the cmdscale() function
mds.var.per <- round(mds.Nations$eig/sum(mds.Nations$eig)*100, 1) # Calculating the amount of variation for each axis in the MDS plot which accounts for using the eigen values
mds.var.per

##### Transform the data for GGplot visualization #####
mds.values <- mds.Nations$points
mds.data_Nations <- data.frame(Nations = rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2])

mds.data_Nations # Final data for using GGplot

##### Creating the Graph using GGplot #####
ggplot(data = mds.data_Nations, aes(x = X, y = Y, label = Nations)) +
  geom_text() +
  theme_bw() +
  xlab(paste("MDS1 -", mds.var.per[1], "%", sep = "")) +
  ylab(paste("MDS2 -", mds.var.per[2], "%", sep = "")) +
  ggtitle("MDS Plot Using Euclidean Distance")