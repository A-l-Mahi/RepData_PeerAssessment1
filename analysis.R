library(ggplot2)
library(lubridate)

if(!file.exists("data")){
        download.file(
                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile = "data/activity.csv"
                )
}

data <- read.csv("data/activity.csv")

data$date <- ymd(data$date)

stepday <- tapply(data$steps, data$date, sum, na.rm = T)
qplot(stepday, xlab = "number of steps each day", ylab = "Frequency")

meanSteps <- mean(stepday)
mediansteps <- median(stepday)