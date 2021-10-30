library(impute)
library(ggplot2)
library(lubridate)
library(Hmisc)


if(!file.exists("data")){
        download.file(
                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile = "activity.csv"
                )
}

data <- read.csv("activity.csv")
data
data$date <- ymd(data$date)

stepday <- tapply(data$steps, data$date, sum, na.rm = T)
qplot(stepday, xlab = "number of steps each day", ylab = "Frequency")

meanSteps <- mean(stepday)
mediansteps <- median(stepday)

avgday <- aggregate(data$steps, by = list(data$interval), mean, na.rm = T)

plot(avgday, type = "l", xlab = "5-minute interval", 
     ylab = "Average Number of steps", col = "dark red")


max(avgday[,1])

length(which(is.na(data)))

data$steps <- impute(data$steps, median)


