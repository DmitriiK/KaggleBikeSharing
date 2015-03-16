# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(Metrics)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/Dimas/Documents/R/KaggleBikeSharing/")

# Load the test and training data
TestData <- read.csv(file = "test.csv")
RawData <- read.csv(file = "train.csv")

dataProcess <- function(df) {
        # Extract the features from datetime variable: hour, month, year, weekday
        df$datetime       = as.character(df$datetime)
        df$datetime       = as.POSIXlt(df$datetime, format="%Y-%m-%d %H:%M:%S", tzone = "EST")
        df$hour           = unclass(df$datetime)[[3]]
        df$month          = unclass(df$datetime)[[5]]
        df$year           = unclass(df$datetime)[[6]]
        df$weekday        = unclass(df$datetime)[[7]]
        
        # Treat the hour, month, year, weekday, season, holiday, workingday and weather as factors
        df$hour           <- factor(df$hour)
        df$month          <- factor(df$month)
        df$year           <- factor(df$year)
        df$weekday        <- factor(df$weekday)
        df$season         <- factor(df$season)
        df$holiday        <- factor(df$holiday)
        df$workingday     <- factor(df$workingday)
        df$weather        <- factor(df$weather)
        
        # return the processed data frame
        return(df)
}


# Applying our data processing function
RawData <- dataProcess(RawData)
TestData <- dataProcess(TestData)

# Treat the count variable, our responce, as an integer. 
RawData$count           <- as.integer(RawData$count)
# Basically we just want to have this column in the test set as well, so it's all zeroes.
TestData$count          <- as.integer(rep(0,6493))


# Remove the original datetime column from both test and training data
RawData         <- RawData[-(1)]
TestData        <- TestData[-(1)]

#Save the prepared data
save(file="TestData.rda", x=TestData)
save(file="RawData.rda", x=RawData)
