---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---

```r
echo = TRUE
```
#### Loading and preprocessing the data

```r
mydata <- read.csv(unz("activity.zip", "activity.csv"))
mydata$date <- as.Date(mydata$date)
mydata$interval <- as.numeric(mydata$interval)
mydata$steps <- as.numeric(mydata$steps)
```
#### What is mean total number of steps taken per day?
##### Make a histogram of the total number of steps taken each day:


```r
sums <- aggregate(cbind(steps) ~ date, data = mydata,  FUN = sum)
hist(sums$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

#####Calculate and report the mean and median total number of steps taken per day:

```r
medians <- aggregate(cbind(steps) ~ date, data = mydata,  FUN = median)
means <- aggregate(cbind(steps) ~ date, data = mydata,  FUN = mean)
```
####### Mean per day:

```r
mean_per_day <- mean(means$steps)
mean_per_day
```

```
## [1] 37.38
```
####### Median per day:

```r
median_per_day <- mean(medians$steps)
median_per_day
```

```
## [1] 0
```

#### What is the average daily activity pattern?
##### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
means_across_days <- aggregate(cbind(steps) ~ interval, data = mydata,  FUN = mean)

plot(means_across_days$interval, means_across_days$steps, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <- max(means_across_days$steps)
which_interval <- means_across_days[means_across_days$steps == max_steps,]$interval
print(which_interval)
```

```
## [1] 835
```

#### Imputing missing values
##### 1) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
nrow(mydata[!complete.cases(mydata),])
```

```
## [1] 2304
```
##### 2-3)  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mydata_with_filled_in_NAs <- mydata

replaceNAs <- function(df1, df2){
   rez <- df1$steps 
   nr <- nrow(df1)
   for(i in 1:nr){
      if(is.na(df1$steps[i])) {
          mean_for_this_interval <- 
              mean(df2[df2$interval == df1$interval[i],]$steps)
          rez[i] <- mean_for_this_interval     
      }
   
      
   }
   
   return(rez)
}

 

mydata_with_filled_in_NAs$steps <- replaceNAs(mydata_with_filled_in_NAs, means_across_days)
```
##### 4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sums2 <- aggregate(cbind(steps) ~ date, data = mydata_with_filled_in_NAs,  FUN = sum)
hist(sums2$steps)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
##### Calculate and report the mean and median total number of steps taken per day:

```r
medians2 <- aggregate(cbind(steps) ~ date, data = mydata_with_filled_in_NAs,  FUN = median)
means2 <- aggregate(cbind(steps) ~ date, data = mydata_with_filled_in_NAs,  FUN = mean)
```
####### Mean per day:

```r
mean_per_day2 <- mean(means2$steps)
mean_per_day2
```

```
## [1] 37.38
```
####### Median per day:

```r
median_per_day2 <- mean(medians2$steps)
median_per_day2
```

```
## [1] 4.474
```
####### Differences:

```r
mean_per_day - mean_per_day2  
```

```
## [1] 0
```

```r
median_per_day - median_per_day2  
```

```
## [1] -4.474
```
#### Are there differences in activity patterns between weekdays and weekends?
##### Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
###### wwd - the name of our new factor variable

```r
wwd <- as.factor( (weekdays(mydata_with_filled_in_NAs$date)==weekdays(as.Date("2001-01-06")) | (weekdays(mydata_with_filled_in_NAs$date)==weekdays(as.Date("2001-01-07")))))

levels(wwd)[levels(wwd)=="TRUE"] <- "weekend"
levels(wwd)[levels(wwd)=="FALSE"] <- "weekday"

mydata_with_filled_in_NAs$wwd <- wwd
```
##### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
means_across_days_for_filled_in_NAs <- aggregate(cbind(steps) ~ interval + wwd, data = mydata_with_filled_in_NAs,  FUN = mean)

library(lattice) 
xyplot(means_across_days_for_filled_in_NAs$steps~means_across_days_for_filled_in_NAs$interval|means_across_days_for_filled_in_NAs$wwd, ylab="Number of steps",    xlab="Interval", layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 
