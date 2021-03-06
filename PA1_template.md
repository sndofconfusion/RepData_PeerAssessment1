# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

The data is loaded into a variable called **activity**, assuming that the file is called "activity.csv" and is in a subfolder of the current working directory called "data".


```r
activity <- read.csv("data/activity.csv")
```

The date column is converted from type factor to type date to help with processing later.


```r
library(dplyr)
activity <- mutate(activity, date = as.Date(date))
```
  
  
  
## What is mean total number of steps taken per day?

Use group_by and summarise functions to obtain the total number of steps per day:


```r
activityByDay <- group_by(activity, date) %>% 
                summarise(total = sum(steps, na.rm=TRUE))
```


The following histogram shows the frequency of total number of steps per day:


```r
library(ggplot2)

ggplot(
        data=activityByDay, 
        aes(total)
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

![](figure/plotFrequencyTotalSteps-1.png)


The mean and median of the total number of steps taken per day are calculated as follows:


```r
meanTotal <- round(mean(activityByDay$total), digits = 0)
medianTotal <- median(activityByDay$total)
```

This gives the mean as 9354 and the median as 10395.  



## What is the average daily activity pattern?

The data is grouped by each 5 minute interval, and the average for each interval is calculated.  This is then plotted in the following graph:


```r
activityByInterval <- group_by(activity, interval) %>% 
                        summarise(average = mean(steps, na.rm=TRUE))

ggplot(
        data=activityByInterval, 
        aes(interval, average)
    ) + 
    geom_line(
        col="darkgreen"
    ) + 
    labs(
        x="Interval", 
        y="Average across all days"
    )
```

![](figure/plotMeanForIntervals-1.png)


The 5-minute interval, which on average across all the days in the dataset, contains the maximum number of steps is calculated as follows


```r
maxSteps <- filter(activityByInterval, average == max(activityByInterval$average))$interval
```

This gives the interval 835.



## Imputing missing values

The total number missing values is calculated as follows:


```r
totalNA <- filter(activity, is.na(steps)) %>% summarise(total = n())
```

This gives the total number if missing values as 2304.


The missing values can be replaced using the average value for that 5 minute interval across the whole period.  This should be rounded to the nearest integer as the number steps should not contain decimals.

Using this strategy, a new dataset called **activityFixNA** is created with no missing values:



```r
activityFixNA <- inner_join(
                    activity, 
                    activityByInterval, 
                    by=c("interval" = "interval")
                ) %>% 
                mutate(
                    steps = ifelse(
                        is.na(steps), 
                        round(average, digits=0), 
                        steps
                    )
                ) %>% 
                select(steps, date, interval)
```


If we group by date, the following histogram shows the frequency of total number of steps per day, now that the missing values have been removed:


```r
activityByDayFixNA <- group_by(activityFixNA, date) %>% 
                        summarise(total = sum(steps))

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

![](figure/plotMeanForIntervalsFixNA-1.png)


The new mean and median are calculated as follows:


```r
meanTotalFixNA <- round(mean(activityByDayFixNA$total), digits = 0)
medianTotalFixNA <- median(activityByDayFixNA$total)
```

This gives the mean as 10766 and the median as 10762.  

By imputing the missing data, both the mean and median values are calculated as higher numbers and the difference is smaller.




## Are there differences in activity patterns between weekdays and weekends?

A new factor variable **dayType** is created to split the data between weekday and weekend:


```r
activityFixNA <- mutate(
                    activityFixNA, 
                    dayType = factor(
                        ifelse(
                            weekdays(date)=="Saturday" | weekdays(date)=="Sunday", 
                            "weekend", 
                            "weekday"
                        )
                    )
                )
```


The following panel plot shows the difference in the average number of steps per 5 minute interval between weekdays and weekends:


```r
activityByIntervalFixNA <- group_by(activityFixNA, interval, dayType) %>% 
                            summarise(average = mean(steps))

library(lattice)

xyplot(
    average ~ interval | dayType, 
    data=activityByIntervalFixNA, 
    type="l", 
    xlab="Interval", 
    ylab="Number of steps", 
    layout=c(1,2), 
    col="darkgreen"
)
```

![](figure/plotCompareDayType-1.png)
