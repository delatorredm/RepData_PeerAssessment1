---
title: 'Peer-graded Assignment: Course Project 1'
author: "Daniel dela Torre"
date: "02/07/2018"
output: html_document
---



### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. $\color{red}{\verb|read.csv()|}$)
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("~/RepData/RepData_PeerAssessment1/")
amd <- read.csv("activity.csv")
amd$date <- as.Date(amd$date) # change date data to date format
```

### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

```r
amdFin <- subset(amd, !is.na(amd$steps))
totalSteps <- aggregate(steps ~ date, data = amdFin, sum)
totalSteps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
ggplot(totalSteps, aes(x=steps)) + 
  geom_histogram(binwidth = 1000, fill = "green")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" width="672" />

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. $\color{red}{\verb|type = "l"|}$) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(ggplot2)
meanInterval <- aggregate(steps ~ interval, data = amdFin, mean)
ggplot(meanInterval, aes(interval, steps)) +
  geom_line(color = "red")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" width="672" />

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
meanInterval$interval[meanInterval$steps == max(meanInterval$steps)]
```

```
## [1] 835
```

### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as $\color{red}{\verb|NA|}$). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with $\color{red}{\verb|NA|}$s)

```r
colSums(is.na(amd))
```

```
##    steps     date interval 
##     2304        0        0
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The 5-minute interval average will be used to fill in $\color{red}{\verb|NA|} values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
amdImputed <- amd
for (i in meanInterval$interval) {
  amdImputed[amdImputed$interval == i & is.na(amdImputed$steps), ]$steps <- meanInterval$steps[meanInterval$interval == i]
}
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
library(ggplot2)
imputedSum <- aggregate(steps ~ date, data = amdImputed, sum)
ggplot(imputedSum, aes(x=steps)) +
  geom_histogram(binwidth = 1000, fill = "red")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-9-1.png" width="672" />

```r
mean(imputedSum$steps)
```

```
## [1] 10766.19
```

```r
median(imputedSum$steps)
```

```
## [1] 10766.19
```

The values do not differ considerably. However, the median values increased and became closer to the mean value.

### Are there differences in activity patterns between weekdays and weekends?

For this part the $\color{red}{\verb|weekdays()|}$ function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekdayData <- amdImputed
weekdayData$wday <- weekdays(weekdayData$date)
weekdayData$dateType <- ifelse(weekdayData$wday == "Saturday" | weekdayData$wday == "Sunday", "weekend", "weekday")
```

2. Make a panel plot containing a time series plot (i.e. $\color{red}{\verb|type = "l"|}$) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(ggplot2)
weekdayInterval <- aggregate(steps ~ interval + dateType, data = weekdayData, mean)
ggplot(weekdayInterval, aes(interval, steps)) + 
    geom_line(color = "red")+ 
    facet_grid(dateType ~ .) +
    xlab("Interval (min)") + ylab("Average Number of Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" width="672" />
