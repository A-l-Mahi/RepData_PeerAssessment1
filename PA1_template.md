---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



## Loading and preprocessing the data

```r
if(!file.exists("data")){
        download.file(
        "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",
                destfile = "activity.zip")
}

unzip("activity.zip")
data <- read.csv("activity.csv")

data$date <- ymd(data$date)
```


## What is mean total number of steps taken per day?


```r
stepday <- tapply(data$steps, data$date, sum, na.rm = TRUE)
```

##### 1. Make a histogram of the total number of steps taken each day

```r
qplot(stepday, xlab = "number of steps each day", ylab = "Frequency",
      binwidth = 300, ylim = c(0, 8))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
meanSteps <- mean(stepday)
medianSteps <- median(stepday)
```
- 9354.2295082
- 10395

## What is the average daily activity pattern?

```r
avgday <- aggregate(data$steps, by = list(data$interval), mean, na.rm = T)
```

#### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
plot(avgday, type = "l", xlab = "5-minute interval", 
     ylab = "Average Number of steps", col = "dark red")
```

![](PA1_template_files/figure-html/time-1.png)<!-- -->

#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max(avgday[,1])
```

```
## [1] 2355
```

## Imputing missing values

#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length(which(is.na(data)))
```

```
## [1] 2304
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data$steps <- impute(data$steps, mean)
```

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data$steps <- impute(data$steps, mean)
```


#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
step.imp <- tapply(data$steps, data$date, sum)
qplot(step.imp, xlab = "number of steps each day", ylab = "Frequency",
      binwidth = 300)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
meanSteps.imp <- mean(step.imp)
medianSteps.imp <- median(step.imp)
```

- 1.0766189\times 10^{4}
- 1.0766189\times 10^{4}

###### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- Apparently Yes!
- The overall observation seem to grow by a small margin and observations of steps  at interval 0 went down.



## Are there differences in activity patterns between weekdays and weekends?


```r
days <- which(weekdays(data$date) %in%
                      c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
end <- which(weekdays(data$date) %in%
                     c("Saturday", "Sunday"))

data[end,4] <- "weekend" 
data[days,4] <- "weekday"
```

#### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data[,4] <- as.factor(data[,4])
colnames(data) <- c("steps", "date", "interval", "week")
```


#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
`

```r
avgday.imp <- aggregate(steps ~ interval + week, mean, data = data)


xyplot(log10(steps) ~ interval | week, data = data,layout=c(1,2),
       type = "l", xlim = c(500, 2000), strip = T, xlab = "5-minute interval",
       ylab = "Average of weekdays/weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->




