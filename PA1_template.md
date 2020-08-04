---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
* Load the data 
* Process/transform the data (if necessary) into a format suitable for your analysis


```r
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date)
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## 1. What is mean total number of steps taken per day?
* Calculate the total number of steps taken per day
* Make a histogram of the total number of steps taken each day




```r
tot_steps<-aggregate(activity$steps,by=list(activity$date),sum,na.rm=TRUE)
names(tot_steps)<-c("date","steps")
hist(tot_steps$steps,breaks =11,main="Total number of steps taken per day",xlab="Total steps taken per day",col='skyblue3',ylim=c(0,20))
```

![](PA1_template_files/figure-html/steps-1.png)<!-- -->

* Calculate and report the mean and median of the total number of steps taken per day

Here is the mean of the total number of steps taken per day:

```r
mean(tot_steps$steps)
```

```
## [1] 9354.23
```
Here is the median of the total number of steps taken per day:

```r
median(tot_steps$steps)
```

```
## [1] 10395
```
## 2. What is the average daily activity pattern?
* Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps<-aggregate(activity$steps,by=list(activity$interval),na.rm=TRUE,mean)
names(average_steps)<-c("interval","steps")
plot(average_steps$interval,average_steps$steps,type="l",xlab="Interval",ylab="Average number of steps",main="Average number of steps per interval",lwd=2,col="skyblue3")
```

![](PA1_template_files/figure-html/average-1.png)<!-- -->

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps[which.max(average_steps$steps),]$interval
```

```
## [1] 835
```

## 3. Imputing missing values
* Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
imputed_steps<-average_steps$steps[match(activity$interval,average_steps$interval)]
```
* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_imputed<-transform(activity,steps=ifelse(is.na(activity$steps),yes=imputed_steps,no=activity$steps))
```
* Make a histogram of the total number of steps taken each day and 

```r
tot_steps_imputed<-aggregate(activity_imputed$steps,by=list(activity_imputed$date),sum,na.rm=TRUE)
names(tot_steps_imputed)<-c("date","steps")
hist(tot_steps_imputed$steps,breaks =11,main="Total number of steps taken per day",xlab="Total steps taken per day",col='skyblue3',ylim=c(0,20))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

* Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Here is the mean of the total number of steps taken per day:

```r
mean(tot_steps_imputed$steps)
```

```
## [1] 10766.19
```

Here is the median of the total number of steps taken per day:

```r
median(tot_steps_imputed$steps)
```

```
## [1] 10766.19
```

The values are slightly different from the first part of the assignment.

## 4. Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_imputed$day<-ifelse(weekdays(activity_imputed$date)=="Sunday"|weekdays(activity_imputed$date)=="Saturday",yes="weekend",no="weekday")
```
* Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
average_steps_imputed<-aggregate(activity_imputed$steps,by=list(activity_imputed$interval),na.rm=TRUE,mean)
average_steps_imputed<-cbind(average_steps_imputed,activity_imputed$day)
names(average_steps_imputed)<-c("interval","steps","day")
p<-ggplot(average_steps_imputed,aes(x=interval,y=steps,color=day))+
        geom_line()+
        labs(title ="Average dailiy steps by type of date",x="Interval",y="Average number of steps")+
        facet_grid(day~.)+
        theme_bw()
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
