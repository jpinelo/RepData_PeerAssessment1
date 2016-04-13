# Reproducible Research: Peer Assessment 1

___
####Loading Libraries       

```r
library(dplyr)
library(ggplot2)
```


## Loading and preprocessing the data

```r
file1 <- unzip("activity.zip")
imp <- read.csv(file1)
df <- imp
df$date <- as.Date(df$date, format = "%Y-%m-%d")
```
#Research Questions and Answers


## What is mean total number of steps taken per day?
*Note:* For this part of the assignment, you can ignore the missing values in the dataset.

#####1.1 - Calculate the total number of steps taken per day.       

```r
stepsPerDay <- df %>%
  group_by(date) %>%
  summarise(daily.steps = sum(steps, na.rm = TRUE))
```

#####1.2 - Make a histogram of the total number of steps taken each day.       

```r
hist(stepsPerDay$daily.steps,
      main="Histogram of the Total Number of Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

#####1.3 - Calculate and report the mean and median total number of steps taken per day.     

```r
meanDailySteps <- mean(stepsPerDay$daily.steps, na.rm = TRUE)
medianDailySteps <- median(stepsPerDay$daily.steps, na.rm = TRUE)
```
The **mean** of daily steps was 9354 steps, and the **median** was 10395 steps.


## What is the average daily activity pattern?
 
#####2.1 - Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).      

```r
averageInterval <- df %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))

qplot(x=interval, y=steps, data=averageInterval, geom="line",
      xlab="5 minute intervals", ylab="Mean number of steps",
      main="Average Number of Steps per Time Interval Accross all Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#####2.2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?     
The 5-minute interval with the highest average number of steps accross all days is interval: **835**     


##Imputing missing values
  
*Note:* Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.      

#####3.1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰     

```r
completeCases <- df[complete.cases(df),]
rowsNAs <- (nrow(df) - nrow(completeCases))
```
There are 2304 missing values in the data set. These are rows with NA values.      

#####3.2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.          
Asigning the mean for the day would imply the elimination of days without any values (such as 2012-10-01), which is not compatibel with what is asked "to make a dataset that is equal to the original but with missing data filled in."
Therefore, the strategy used is to replace each NA value in the column `steps` by the average of the corresponding interval over all days. 

#####3.3 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.           

```r
df$steps <- as.numeric(df$steps)
df$interval <- as.numeric(df$interval)

dfImputed  <- df %>%
  group_by(interval) %>%
   mutate_each(funs(replace(., is.na(.), mean(., na.rm = TRUE))), -date)
head(dfImputed)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
The equivalent dataset is `dfImputed`, calculated and partially displayed above.      

#####3.4 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?           

```r
dfImputedPerDay <- dfImputed %>%
  group_by(date) %>%
  summarise(daily.steps = sum(steps))
hist(dfImputedPerDay$daily.steps, main = "Histogram of the Total Number of Daily Steps (with imputed data)")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
meanDailyStepsImputed <- mean(dfImputedPerDay$daily.steps)
medianDailyStepsImputed <- median(dfImputedPerDay$daily.steps)
```
The **mean** of daily steps was 10766 steps, and the **median** was 10766 steps.     
Note that the mean and median are now closer than before the data was imputed. The data is now less skewed, more 'normally' disributed, which is confirmed by comparing the histograms. There are less events within 0-5000 bin, and more into 10000-15000. Since mean values were introduced, the whole data is more centred around the mean. This happened assymetriccally, meaning only lower end class was affected as NAs were replaced.


## Are there differences in activity patterns between weekdays and weekends?
    
*Note:* Use the dataset with the filled-in missing values for this part.       

#####4.1 - Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.     


```r
dfImputedDays <- dfImputed %>%
  mutate(day = as.factor(ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", yes = "weekend", no = "weekday")))
levels(dfImputedDays$day)
```

```
## [1] "weekday" "weekend"
```
As can be confirmed by the output of `str(dfImputedDays)` above, dataset `dfImputedDays` contains the new variable `day`, containing the description for each row as belonging to weekdays or weekends.


#####4.2 - Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).       


```r
intervalPattern <- dfImputedDays %>%
  group_by(interval, day) %>%
  summarise(steps.interval = mean(steps))

qplot(x = interval, y = steps.interval, data = intervalPattern, facets = day ~ ., geom = "line",
      main = "Daily Pattern of Steps By Interval on Weekdays and Weekends", ylab = "Mean Steps")     
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
        
The plot above shows the differet patterns of averaged weekday and weeekend number of steps per interval. Notice, for example, that:     

* On weekdays, the number of steps increases sharply just after interval 500, while on the weekends, there is a more progressive increase between intervals 500 and 750;     
* Weekends show less intense activity (steps) peaks (approximately 170 max against 260 max number of steps per 5 minutes);
* Weekend days  present a more 'rugged activity pattern' with a higher range of values troughout the day. Just through visual inspection, on weekdays, most activity falls below 50 steps/5min, while on weekend days this threshold is 100 steps/5min.     
* On weekdays there are only two peaks over 100steps/5min, while on weekends there are nine.    
