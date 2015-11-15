---
title: "Reproducible Reasearch Assignment"
author: "MichaÅ‚ Libura"
date: "14 listopada 2015"
output: html_document
---

#Loading and preprocessing the data

1) Loading data and converting data$date into date format using Lubridate package

```r
data = read.csv("activity.csv")
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.2
```

```r
data$date = ymd(data$date)
```

#What is mean total number of steps taken per day?

1) Calculate the total number of steps taken per day

```r
sum(data$steps, na.rm = TRUE)
```

```
## [1] 570608
```

2) Make a histogram of the total number of steps taken each day

```r
hist(tapply(data$steps, as.factor(data$date), sum, na.rm=T), breaks = 30)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3) Calculate and report the mean and median of the total number of steps taken per day  
Median:

```r
median(data$steps, na.rm=T)
```

```
## [1] 0
```
Mean:

```r
mean(data$steps, na.rm=T)
```

```
## [1] 37.3826
```

#What is the average daily activity pattern?

1) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## Nastêpuj¹ce obiekty zosta³y zakryte z 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## Nastêpuj¹ce obiekty zosta³y zakryte z 'package:stats':
## 
##     filter, lag
## 
## Nastêpuj¹ce obiekty zosta³y zakryte z 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data$interval = as.factor(data$interval)
x = data %>% group_by(interval) %>% summarise(step.mean = mean(steps, na.rm=T))
plot(as.numeric(x$interval), x$step.mean, type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
 x$interval[which.max(x$step.mean)]
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

#Imputing missing values

1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval.


```r
x$interval = as.factor(x$interval)
data$interval = as.factor(data$interval)
replace = function(){
        newdata = data.frame()
        nalogic = is.na(data$steps)
        for (i in 1:length(nalogic)){
                if (nalogic[i] == TRUE){
                        a = data$interval[i]
                        ver = data[i,]
                        ver$steps = x$step.mean[x$interval==a]
                        newdata = rbind(newdata, ver)
                }
                if (nalogic[i]==FALSE){
                        ver = data[i,]
                        newdata = rbind(newdata, ver)
                }
                
        }
        return(newdata)
}
```


3) Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newdata = replace()
```

4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(tapply(newdata$steps, newdata$date, sum), breaks = 30)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
mean(newdata$steps)
```

```
## [1] 37.3826
```

```r
median(newdata$steps)
```

```
## [1] 0
```

#Are there differences in activity patterns between weekdays and weekends?

1) Create a new factor variable in the dataset with two levels â€“ â€œweekdayâ€ and â€œweekendâ€ indicating whether a given date is a weekday or weekend day.


```r
newdata$date = ymd(newdata$date)
newdata$day = wday(newdata$date)
newdata$type = ifelse(newdata$day==6 | newdata$day==7, "weekend", "weekday")
```

2) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(lattice)        
x = newdata %>% group_by(type,interval) %>% summarise(step.mean = mean(steps))
xyplot(as.numeric(step.mean)~as.numeric(interval)|type,data=x, type="l", layout = c(1,2), ylab="Number of steps", xlab = "Interval")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 


