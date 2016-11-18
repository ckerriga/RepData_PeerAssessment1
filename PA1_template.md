# Reproducible Research: Peer Assessment 1
Chris Kerrigan  
November 18, 2016  


## Loading and preprocessing the data
Below are the steps required to read in the activity data. Since the data is already tidy, there is no need for any preprocessing at the moment. Any manipulations to the data required for future steps will be performed at that time.


```r
setwd('C:/Users/ckerriga/Documents/Coursera/Project Data')
data <- read.csv('activity.csv')
```

## What is mean total number of steps taken per day?
For this part of the assignment, we wish to ignore missing values in the dataset. We will do this by creating a new data set called **dataNoNA**. After creating this new data set, we will create a histogram (using R base plotting system) of the total number of steps taken each day.


```r
dataNoNA <- data[!(is.na(data$steps)),]
dataByDay <- aggregate(dataNoNA$steps, by = list(dataNoNA$date), sum)
names(dataByDay) <- c('date', 'sum_of_steps')
hist(dataByDay$sum_of_steps, col = "blue", main = "Total Steps Taken Each Day", xlab = "Total Steps per Day", ylab = "Frequency (in days)", breaks = 10)
```

![](PA1_template_files/figure-html/noNAs-1.png)<!-- -->

Now we want to calculate the mean and median total number of steps taken per day. 


```r
meanStepsPerDay <- mean(dataByDay$sum_of_steps)
medianStepsPerDay <- median(dataByDay$sum_of_steps)
```
From this we can see that the **mean** number of steps per day is **1.0766189\times 10^{4}** and the **median** number of steps per day is **10765**. 


## What is the average daily activity pattern?
Now we will make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days. 


```r
dataByInterval <- aggregate(dataNoNA$steps, by = list(dataNoNA$interval), mean)
names(dataByInterval) <- c('interval', 'avg_steps')
with(dataByInterval, plot(interval, avg_steps, type = 'l', main = 'Avg Steps Taken by Time Interval'))
```

![](PA1_template_files/figure-html/timeSeries1-1.png)<!-- -->

Perform the calculation below to determine which 5-minute interval throughout the day contains the maximum number of steps, on average:


```r
max <- max(dataByInterval$avg_steps)
intervalWithMaxSteps <- dataByInterval[dataByInterval$avg_steps == max,][1,1]
```

From the above calculation, we can see that the 5-minute interval (averaged across all days) containing the maximum number of steps is **835**. This is consistent with the graph above, because the spike appears to occur between intervals 750 and 1000.

## Imputing missing values
First we would like to computing the total number of missing values (coded as NA) in the dataset. The returned value after the code chunk below is the count of NAs:


```r
count <- 0
for(row in seq(length(data$steps))) {
  if(is.na(data$steps[row])) {
    count <- count + 1
  }
}
count
```

```
## [1] 2304
```

In order to remove potential bias from our data set, we will replace all 2304 NA values with the mean for that 5-minute interval across all days. 


```r
merged <- merge(x = data, y = dataByInterval, by = 'interval')
merged$non_na_steps <- ifelse(is.na(merged$steps), merged$avg_steps, merged$steps)
dataImputed <- merged[,c('non_na_steps', 'date', 'interval')]
```

Now we will make a histogram similar to the one we made above, except this time we will use the new imputed dataset. 


```r
dataImputedByDay <- aggregate(dataImputed$non_na_steps, by = list(dataImputed$date), sum)
names(dataImputedByDay) <- c('date', 'sum_of_steps')
hist(dataImputedByDay$sum_of_steps, col = "grey", main = "Total Steps Taken Each Day", xlab = "Total Steps per Day", ylab = "Frequency (in days)", breaks = 10)
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

```r
meanImputed <- mean(dataImputedByDay$sum_of_steps)
medianImputed <- median(dataImputedByDay$sum_of_steps)
```



Our calculations above show that the **mean** and **median** number of steps taken per day are **1.0766189\times 10^{4}** and **1.0766189\times 10^{4}**, respectively. We notice that the only major difference between this histogram and the one we made by excluding NA values is the y-axis. Previously we created a histogram of 15264 rows of data, and now we created one for 17568 rows of data (because there are 2304 NA rows.) The distribution of steps throughout the day is the same as before since we replaced all NA values with the average number of steps for each interval. Thus, the average number of steps for each interval in not changing, so the only change in the histogram will be the total number of observations, which are counted on the y-axis. 

## Are there differences in activity patterns between weekdays and weekends?
Finally, we would like to understand the difference in activity patterns between weekdays and weekends. The code chunk below creates a new factor variable indicating whether a record is from the weekday or the weekend:


```r
dataImputed$day_type <- as.factor(ifelse(weekdays(as.Date(dataImputed$date)) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday'))
head(dataImputed)
```

```
##   non_na_steps       date interval day_type
## 1     1.716981  10/1/2012        0  weekday
## 2     0.000000 11/23/2012        0  weekday
## 3     0.000000 10/28/2012        0  weekday
## 4     0.000000  11/6/2012        0  weekday
## 5     0.000000 11/24/2012        0  weekday
## 6     0.000000 11/15/2012        0  weekday
```

Create side-by-side time series plots of the 5-minute interval and the average number of steps taken, averaged across all weekday days and weekend days. 


```r
par(mfcol = c(2, 1), oma = c(4, 4, 2, 1))

# create the weekend graph first
weekend <- dataImputed[dataImputed$day_type == "weekend",]
weekendAgg <- aggregate(weekend$non_na_steps, by = list(weekend$interval), mean)
names(weekendAgg) <- c('interval', 'avg_steps')
with(weekendAgg, plot(interval, avg_steps, type = 'l', col = 'blue', main = "Weekend", xlab = "Interval", ylab = "Number of Steps"))

# now create the weekday graph
weekday <- dataImputed[dataImputed$day_type == "weekday",]
weekdayAgg <- aggregate(weekday$non_na_steps, by = list(weekday$interval), mean)
names(weekdayAgg) <- c('interval', 'avg_steps')
with(weekdayAgg, plot(interval, avg_steps, type = 'l', col = 'blue', main = "Weekday", xlab = "Interval", ylab = "Number of Steps"))
```

![](PA1_template_files/figure-html/activity_comparisons-1.png)<!-- -->




