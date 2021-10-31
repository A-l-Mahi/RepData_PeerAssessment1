library(ggplot2)
library(lubridate)
library(Hmisc)
library(lattice)


if(!file.exists("data")){
        download.file(
                "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                      destfile = "activity.zip"
                )
}
unzip("activity.zip")
data <- read.csv("activity.csv")

data$date <- ymd(data$date)

stepday <- tapply(data$steps, data$date, sum, na.rm = T)
qplot(stepday, xlab = "number of steps each day", ylab = "Frequency",
      binwidth = 300)

meanSteps <- mean(stepday)
mediansteps <- median(stepday)

avgday <- aggregate(data$steps, by = list(data$interval), mean)

plot(avgday, type = "l", xlab = "5-minute interval", 
     ylab = "Average Number of steps", col = "dark red")

max(avgday[,1])

length(which(is.na(data)))

data$steps <- impute(data$steps, mean)

step.imp <- tapply(data$steps, data$date, sum, na.rm = T)
qplot(step.imp, xlab = "number of steps each day", ylab = "Frequency",
      binwidth = 300)

meanSteps.imp <- mean(step.imp)
mediansteps.imp <- median(step.imp)

days <- which(weekdays(data$date) %in%
                      c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
end <- which(weekdays(data$date) %in%
                     c("Saturday", "Sunday"))

data[end,4] <- "weekend" 
data[days,4] <- "weekday"

data[,4] <- as.factor(data[,4])
class(data[,4])

colnames(data) <- c("steps", "date", "interval", "week")


avgday.imp <- aggregate(steps ~ interval + week, mean, data = data)

xyplot(log2(steps) ~ log2(interval) | week, data = data,layout=c(1,2),
       type = "l", strip = T, xlim = c(9,11))

