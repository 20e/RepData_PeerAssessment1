# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("activity.zip")
activity<-read.csv("activity.csv")
```
## What is mean total number of steps taken per day?
### Histogram of the total number of steps taken each day

```r
activity_rm<-na.omit(activity)
sum_day<-na.omit(data.frame(tapply(activity_rm$steps,activity_rm$date,sum)))
sum_day<-data.frame(sum_day,unique(activity_rm$date))
colnames(sum_day)<-c("sum","date")
hist(sum_day$sum,main="activity",xlab = "total steps per day",ylab = "frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
   
### Mean and median number of steps taken each day  


```r
mean_day<-mean(sum_day$sum)
print(mean_day)
```

```
## [1] 10766.19
```

```r
median_day<-median(sum_day$sum)
print(median_day)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
### Time series plot of the average number of steps taken

```r
mean_interval<-tapply(activity_rm$steps,activity_rm$interval,mean)
mean_interval<-data.frame(mean_interval,unique(activity_rm$interval))
colnames(mean_interval)<-c("mean","interval")
plot(mean_interval$interval,mean_interval$mean,type = "l",xlab = "5-minute interval",ylab = "number of steps",main="average steps across days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
### The 5-minute interval that, on average, contains the maximum number of steps

```r
max_interval<-mean_interval$interval[mean_interval$mean==max(mean_interval$mean)]
print(max_interval)
```

```
## [1] 835
```
## Imputing missing values
 Code to describe and show a strategy for imputing missing data
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
length_NAs<-sum(is.na(activity))
print(length_NAs)
```

```
## [1] 2304
```
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
## My strategy is to replace the NAs with the mean for that 5-interval.

```r
activity1<-activity
for(i in 1:nrow(activity1)){
        if(is.na(activity1[i,1])){
                impute<-subset(mean_interval,interval==activity1[i,3])
                activity1[i,1]<-impute$mean
        }
        i<-i+1
}
```
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
any(is.na(activity1))
```

```
## [1] FALSE
```

```r
str(activity1)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Histogram of the total number of steps taken each day after missing values are imputed

```r
sum_day1<-data.frame(tapply(activity1$steps,activity1$date,sum),unique(activity1$date))
colnames(sum_day1)<-c("sum","date")
hist(sum_day1$sum,main="activity",xlab = "total steps per day",ylab = "frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean_day1<-mean(sum_day1$sum)
variance_mean<-mean_day-mean_day1
print(variance_mean)
```

```
## [1] 0
```

```r
median_day1<-median(sum_day1$sum)
variance_median<-median_day-median_day1
print(variance_median)
```

```
## [1] -1.188679
```
## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity1$date<-as.Date(activity1$date)
for(i in 1:nrow(activity1)){
        if(weekdays(activity1$date[i])%in%c("星期日","星期六")){
                activity1$week[i]<-"weekend"
        }else
                activity1$week[i]<-"weekday"
        i<-i+1
}
```
### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
## PS: the data I imputed is the mean of that interval, which causes the difference between my plot and the plot in README.md.
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activity1_wd<-subset(activity1,activity1$week=="weekday")
activity1_we<-subset(activity1,activity1$week=="weekend")
mean_wd<-data.frame(tapply(activity1_wd$steps,activity1_wd$interval,mean),unique(activity1_wd$interval),"weekday")
colnames(mean_wd)<-c("mean","interval","week")
mean_we<-data.frame(tapply(activity1_we$steps,activity1_we$interval,mean),unique(activity1_we$interval),"weekend")
colnames(mean_we)<-c("mean","interval","week")
mean_week<-rbind(mean_wd,mean_we)

library(lattice)
xyplot(mean~interval|week,data=mean_week,type="l",layout=c(1,2),ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
   
## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report  
