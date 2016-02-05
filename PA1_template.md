# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data
The data is loaded assuming that the file is called "activity.csv"" in a subfolder of the current working directory called data.


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```r
activity <- read.csv("data/activity.csv")

activity <- mutate(activity, date = as.Date(date))
```


## What is mean total number of steps taken per day?

Use group_by and summarise functions to obtain the total number of steps per day:


```r
activityByDay <- group_by(activity, date) %>% summarise(total = sum(steps, na.rm=TRUE))

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.3
```

```r
ggplot(
  data=activityByDay, 
  aes(activityByDay$total)
) +
geom_histogram(
  col="darkgreen",
  fill="palegreen3",
  binwidth=1000
) + 
labs(
  x="Total Steps per Day", 
  y="Frequency"
)
```

![](figure/summariseData-1.png)

```r
meanTotal <- round(mean(activityByDay$total), digits = 0)
medianTotal <- median(activityByDay$total)
```

The mean of the total number of steps taken per day is 9354 and the median is 10395.



## What is the average daily activity pattern?


```r
activityByInterval <- group_by(activity, interval) %>% summarise(average = mean(steps, na.rm=TRUE))

ggplot(data=activityByInterval, aes(interval, average)) + geom_line(col="darkgreen") + labs(x="Interval", y="Average across all days")
```

![](figure/summariseByInterval-1.png)

```r
maxSteps <- filter(activityByInterval, average == max(activityByInterval$average))$interval
```

The 5-minute interval, which on average across all the days in the dataset, contains the maximum number of steps is interval 835.



## Imputing missing values


```r
totalNA <- filter(activity, is.na(steps)) %>% summarise(total = n())



activityFixNA <- inner_join(activity, activityByInterval, by=c("interval" = "interval")) %>% mutate(steps = ifelse(is.na(steps), round(average, digits=0), steps)) %>% select(steps, date, interval)


activityByDayFixNA <- group_by(activityFixNA, date) %>% summarise(total = sum(steps))

ggplot(
  data=activityByDayFixNA, 
  aes(activityByDayFixNA$total)
) +
geom_histogram(
  col="darkgreen",
  fill="palegreen3",
  binwidth=1000
) + 
labs(
  x="Total Steps per Day", 
  y="Frequency"
)
```

![](figure/missingValues-1.png)

```r
meanTotalFixNA <- round(mean(activityByDayFixNA$total), digits = 0)
medianTotalFixNA <- median(activityByDayFixNA$total)
```

The mean of the total number of steps taken per day is 1.0766\times 10^{4} and the median is 1.0762\times 10^{4}.




## Are there differences in activity patterns between weekdays and weekends?


```r
activityFixNA <- mutate(activityFixNA, dayType = factor(ifelse(weekdays(date)=="Saturday" | weekdays(date)=="Sunday", "weekend", "weekday")))

activityByIntervalFixNA <- group_by(activityFixNA, interval, dayType) %>% summarise(average = mean(steps))

library(lattice)

xyplot(average ~ interval | dayType, data=activityByIntervalFixNA, type="l", xlab="Interval", ylab="Number of steps", layout=c(1,2), col="darkgreen")
```

![](figure/weekdays-1.png)


