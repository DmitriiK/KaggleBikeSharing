# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(randomForest)
library(Metrics)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/Dimas/Documents/R/KaggleBikeSharing/")

# Load the test data
TestData <- read.csv(file = "test.csv")

# Extract the features from datetime variable: hour, month, year, weekday
TestData$datetime       = as.character(TestData$datetime)
TestData$datetime       = as.POSIXlt(TestData$datetime, format="%Y-%m-%d %H:%M:%S", tzone = FALSE)
TestData$hour           = unclass(TestData$datetime)[[3]]
TestData$month          = unclass(TestData$datetime)[[5]]
TestData$year           = unclass(TestData$datetime)[[6]]
TestData$weekday        = unclass(TestData$datetime)[[7]]

# Treat the hour, month, year, weekday, season, holiday, workingday and weather as factors
TestData$hour           <- factor(TestData$hour)
TestData$month          <- factor(TestData$month)
TestData$year           <- factor(TestData$year)
TestData$weekday        <- factor(TestData$weekday)
TestData$season         <- factor(TestData$season)
TestData$holiday        <- factor(TestData$holiday)
TestData$workingday     <- factor(TestData$workingday)
TestData$weather        <- factor(TestData$weather)

# Treat the count variable, our responce, as an integer. 
# Basically we just want to have this column in the test set, so it's all zeroes.
TestData$count          <- as.integer(rep(0,6493))


##################

# Repeat the same operations for the provided training data

# Load the training data
RawData <- read.csv(file = "train.csv")

# Extract the features from datetime variable: hour, month, year, weekday
RawData$datetime        = as.character(RawData$datetime)
RawData$datetime        = as.POSIXlt(RawData$datetime, format="%Y-%m-%d %H:%M:%S", tzone = FALSE)
RawData$hour            = unclass(RawData$datetime)[[3]]
RawData$month           = unclass(RawData$datetime)[[5]]
RawData$year            = unclass(RawData$datetime)[[6]]
RawData$weekday         = unclass(RawData$datetime)[[7]]

# Treat the hour, month, year, weekday, season, holiday, workingday and weather as factors
RawData$hour            <- factor(RawData$hour)
RawData$month           <- factor(RawData$month)
RawData$year            <- factor(RawData$year)
RawData$weekday         <- factor(RawData$weekday)
RawData$season          <- factor(RawData$season)
RawData$holiday         <- factor(RawData$holiday)
RawData$workingday      <- factor(RawData$workingday)
RawData$weather         <- factor(RawData$weather)

# Treat the count variable, our responce, as an integer. 
RawData$count           <- as.integer(RawData$count)


# Remove the original datetime column from both test and training data
RawData         <- RawData[-(1)]
TestData        <- TestData[-(1)]

#Save the prepared data
save(file="TestData.rda", x=TestData)
save(file="RawData.rda", x=RawData)
