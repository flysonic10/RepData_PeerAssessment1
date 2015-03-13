---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- aggregate(data$steps, by=list(data$date), FUN=sum)
hist(total_steps_per_day$x, breaks=10, main="Histogram of Steps per Day", xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean_steps_per_day <- aggregate(steps ~ date, data, mean, na.action=na.omit)
median_steps_per_day <- aggregate(steps ~ date, data, median, na.action=na.omit)

mean_steps_per_day
```

```
##          date   steps
## 1  2012-10-02  0.4375
## 2  2012-10-03 39.4167
## 3  2012-10-04 42.0694
## 4  2012-10-05 46.1597
## 5  2012-10-06 53.5417
## 6  2012-10-07 38.2465
## 7  2012-10-09 44.4826
## 8  2012-10-10 34.3750
## 9  2012-10-11 35.7778
## 10 2012-10-12 60.3542
## 11 2012-10-13 43.1458
## 12 2012-10-14 52.4236
## 13 2012-10-15 35.2049
## 14 2012-10-16 52.3750
## 15 2012-10-17 46.7083
## 16 2012-10-18 34.9167
## 17 2012-10-19 41.0729
## 18 2012-10-20 36.0938
## 19 2012-10-21 30.6285
## 20 2012-10-22 46.7361
## 21 2012-10-23 30.9653
## 22 2012-10-24 29.0104
## 23 2012-10-25  8.6528
## 24 2012-10-26 23.5347
## 25 2012-10-27 35.1354
## 26 2012-10-28 39.7847
## 27 2012-10-29 17.4236
## 28 2012-10-30 34.0938
## 29 2012-10-31 53.5208
## 30 2012-11-02 36.8056
## 31 2012-11-03 36.7049
## 32 2012-11-05 36.2465
## 33 2012-11-06 28.9375
## 34 2012-11-07 44.7326
## 35 2012-11-08 11.1771
## 36 2012-11-11 43.7778
## 37 2012-11-12 37.3785
## 38 2012-11-13 25.4722
## 39 2012-11-15  0.1424
## 40 2012-11-16 18.8924
## 41 2012-11-17 49.7882
## 42 2012-11-18 52.4653
## 43 2012-11-19 30.6979
## 44 2012-11-20 15.5278
## 45 2012-11-21 44.3993
## 46 2012-11-22 70.9271
## 47 2012-11-23 73.5903
## 48 2012-11-24 50.2708
## 49 2012-11-25 41.0903
## 50 2012-11-26 38.7569
## 51 2012-11-27 47.3819
## 52 2012-11-28 35.3576
## 53 2012-11-29 24.4688
```

```r
median_steps_per_day
```

```
##          date steps
## 1  2012-10-02     0
## 2  2012-10-03     0
## 3  2012-10-04     0
## 4  2012-10-05     0
## 5  2012-10-06     0
## 6  2012-10-07     0
## 7  2012-10-09     0
## 8  2012-10-10     0
## 9  2012-10-11     0
## 10 2012-10-12     0
## 11 2012-10-13     0
## 12 2012-10-14     0
## 13 2012-10-15     0
## 14 2012-10-16     0
## 15 2012-10-17     0
## 16 2012-10-18     0
## 17 2012-10-19     0
## 18 2012-10-20     0
## 19 2012-10-21     0
## 20 2012-10-22     0
## 21 2012-10-23     0
## 22 2012-10-24     0
## 23 2012-10-25     0
## 24 2012-10-26     0
## 25 2012-10-27     0
## 26 2012-10-28     0
## 27 2012-10-29     0
## 28 2012-10-30     0
## 29 2012-10-31     0
## 30 2012-11-02     0
## 31 2012-11-03     0
## 32 2012-11-05     0
## 33 2012-11-06     0
## 34 2012-11-07     0
## 35 2012-11-08     0
## 36 2012-11-11     0
## 37 2012-11-12     0
## 38 2012-11-13     0
## 39 2012-11-15     0
## 40 2012-11-16     0
## 41 2012-11-17     0
## 42 2012-11-18     0
## 43 2012-11-19     0
## 44 2012-11-20     0
## 45 2012-11-21     0
## 46 2012-11-22     0
## 47 2012-11-23     0
## 48 2012-11-24     0
## 49 2012-11-25     0
## 50 2012-11-26     0
## 51 2012-11-27     0
## 52 2012-11-28     0
## 53 2012-11-29     0
```


## What is the average daily activity pattern?

```r
mean_steps_per_interval <- aggregate(steps ~ interval, data, mean, na.action=na.omit)
plot(mean_steps_per_interval, type='l', xlab="Interval", ylab="Steps", main="Steps per Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The 5 minute interval with the maximum number of steps is obtained with:

```r
max_interval = mean_steps_per_interval$interval[which.max(mean_steps_per_interval$steps)]

max_interval
```

```
## [1] 835
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
number_of_nas <- sum(is.na(data$steps))
number_of_nas
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

NAs will be filled in by the mean of the 5-minute interval rounded to the nearest integer. 

First we create a copy of the dataset.


```r
imputed_data = data
```

Next, we pull out the indices where the interval in the dataset match the interval in the mean step per interval data, and also match the NA values in the dataset.


```r
step_indices <- mean_steps_per_interval$interval == imputed_data$interval[is.na(data$steps)]
```

Finally, we replace the NA values with the mean steps per interval values.


```r
imputed_data$steps[is.na(imputed_data$steps)] = round(mean_steps_per_interval$steps[step_indices])
```

Now, we can create a new histogram based on the imputed data.


```r
imputed_total_steps_per_day <- aggregate(steps ~ date, imputed_data, sum)
hist(imputed_total_steps_per_day$steps, breaks=10, main="Histogram of Steps per Day (imputed)", xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

We do the same for a new mean / median


```r
imputed_mean_steps_per_day <- aggregate(steps ~ date, imputed_data, mean, na.action=na.omit)
imputed_median_steps_per_day <- aggregate(steps ~ date, imputed_data, median, na.action=na.omit)

imputed_mean_steps_per_day
```

```
##          date   steps
## 1  2012-10-01 37.3681
## 2  2012-10-02  0.4375
## 3  2012-10-03 39.4167
## 4  2012-10-04 42.0694
## 5  2012-10-05 46.1597
## 6  2012-10-06 53.5417
## 7  2012-10-07 38.2465
## 8  2012-10-09 44.4826
## 9  2012-10-10 34.3750
## 10 2012-10-11 35.7778
## 11 2012-10-12 60.3542
## 12 2012-10-13 43.1458
## 13 2012-10-14 52.4236
## 14 2012-10-15 35.2049
## 15 2012-10-16 52.3750
## 16 2012-10-17 46.7083
## 17 2012-10-18 34.9167
## 18 2012-10-19 41.0729
## 19 2012-10-20 36.0938
## 20 2012-10-21 30.6285
## 21 2012-10-22 46.7361
## 22 2012-10-23 30.9653
## 23 2012-10-24 29.0104
## 24 2012-10-25  8.6528
## 25 2012-10-26 23.5347
## 26 2012-10-27 35.1354
## 27 2012-10-28 39.7847
## 28 2012-10-29 17.4236
## 29 2012-10-30 34.0938
## 30 2012-10-31 53.5208
## 31 2012-11-02 36.8056
## 32 2012-11-03 36.7049
## 33 2012-11-05 36.2465
## 34 2012-11-06 28.9375
## 35 2012-11-07 44.7326
## 36 2012-11-08 11.1771
## 37 2012-11-11 43.7778
## 38 2012-11-12 37.3785
## 39 2012-11-13 25.4722
## 40 2012-11-15  0.1424
## 41 2012-11-16 18.8924
## 42 2012-11-17 49.7882
## 43 2012-11-18 52.4653
## 44 2012-11-19 30.6979
## 45 2012-11-20 15.5278
## 46 2012-11-21 44.3993
## 47 2012-11-22 70.9271
## 48 2012-11-23 73.5903
## 49 2012-11-24 50.2708
## 50 2012-11-25 41.0903
## 51 2012-11-26 38.7569
## 52 2012-11-27 47.3819
## 53 2012-11-28 35.3576
## 54 2012-11-29 24.4688
```

```r
imputed_median_steps_per_day
```

```
##          date steps
## 1  2012-10-01  34.5
## 2  2012-10-02   0.0
## 3  2012-10-03   0.0
## 4  2012-10-04   0.0
## 5  2012-10-05   0.0
## 6  2012-10-06   0.0
## 7  2012-10-07   0.0
## 8  2012-10-09   0.0
## 9  2012-10-10   0.0
## 10 2012-10-11   0.0
## 11 2012-10-12   0.0
## 12 2012-10-13   0.0
## 13 2012-10-14   0.0
## 14 2012-10-15   0.0
## 15 2012-10-16   0.0
## 16 2012-10-17   0.0
## 17 2012-10-18   0.0
## 18 2012-10-19   0.0
## 19 2012-10-20   0.0
## 20 2012-10-21   0.0
## 21 2012-10-22   0.0
## 22 2012-10-23   0.0
## 23 2012-10-24   0.0
## 24 2012-10-25   0.0
## 25 2012-10-26   0.0
## 26 2012-10-27   0.0
## 27 2012-10-28   0.0
## 28 2012-10-29   0.0
## 29 2012-10-30   0.0
## 30 2012-10-31   0.0
## 31 2012-11-02   0.0
## 32 2012-11-03   0.0
## 33 2012-11-05   0.0
## 34 2012-11-06   0.0
## 35 2012-11-07   0.0
## 36 2012-11-08   0.0
## 37 2012-11-11   0.0
## 38 2012-11-12   0.0
## 39 2012-11-13   0.0
## 40 2012-11-15   0.0
## 41 2012-11-16   0.0
## 42 2012-11-17   0.0
## 43 2012-11-18   0.0
## 44 2012-11-19   0.0
## 45 2012-11-20   0.0
## 46 2012-11-21   0.0
## 47 2012-11-22   0.0
## 48 2012-11-23   0.0
## 49 2012-11-24   0.0
## 50 2012-11-25   0.0
## 51 2012-11-26   0.0
## 52 2012-11-27   0.0
## 53 2012-11-28   0.0
## 54 2012-11-29   0.0
```

Comparing the two histograms, we can see no material effect from imputing data.


```r
par(mfrow=c(1,2))
hist(total_steps_per_day$x, breaks=10, main="Histogram of Steps per Day", xlab="Total Steps per Day")
hist(imputed_total_steps_per_day$steps, breaks=10, main="Histogram of Steps per Day (imputed)", xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


## Are there differences in activity patterns between weekdays and weekends?

To further explore the impact of weekday vs weekend activity, we can factor the data.


```r
imputed_data$day[weekdays(as.POSIXct(imputed_data$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] = "Weekday"
imputed_data$day[weekdays(as.POSIXct(imputed_data$date)) %in% c("Saturday", "Sunday")] = "Weekend"

factored_data = as.factor(imputed_data$day)
```

Finally, we plot the data based on weekday vs weekend.


```r
mean_steps_per_daytype <- aggregate(steps ~ interval + day, imputed_data, mean, na.action=na.omit)

par(mfrow=c(2,1))
par(mar=c(4,4,2,0))
plot(mean_steps_per_daytype$steps[mean_steps_per_daytype$day == "Weekday"], type='l', xlab="Interval", ylab="Steps", main="Steps per Weekday")
plot(mean_steps_per_daytype$steps[mean_steps_per_daytype$day == "Weekend"], type='l', xlab="Interval", ylab="Steps", main="Steps per Weekend")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 




