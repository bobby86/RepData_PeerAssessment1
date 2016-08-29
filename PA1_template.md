Reproducible Research: Week-2 Peer Graded Assignment
====================================================
###submitted by: Pradeep

##Code for reading in the dataset and processing the data
The data is read from the activity.csv file. The date and interval variables are modifed as date and factor datatypes respectively for easier grouping and plotting the data.

```r
activity <- read.csv(file=unzip('./activity.zip'))
activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity$interval <- as.factor(activity$interval)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

##Calculating the total number of steps taken in each day
Using the dplyr package to group the data.


```r
StepsByDay <- group_by(activity, date)
StepsByDay <- summarise(StepsByDay, steps = sum(steps,na.rm = TRUE))
```

##Ploting the Histogram of the total number of steps taken each day

```r
par(mfrow = c(1,1), mar = c(4,4,2,1))
with(StepsByDay, barplot(steps, names.arg=date, border = "black", col="Red", xlab="Date"
                         , ylab="Number of Steps", main="Total Steps per day"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
#Mean and median number of steps taken each day
MeanStepsByDay <- mean(StepsByDay$steps)
MedianStepsByDay <- median(StepsByDay$steps)
```

The mean and median number of steps per day is calculated by ignoring the missing data, The mean is 9354.2295082 and the median is 10395.

##Calculating the average number of steps taken in each interval for Time series plot

```r
StepsByInterval <- group_by(activity, interval)
StepsByInterval <- summarise(StepsByInterval, steps = mean(steps, na.rm = TRUE))
```

##Ploting the Time series plot of the average number of steps taken in each interval

```r
with(StepsByInterval, plot(interval, steps, type = "n", col="Red", xlab="Time"
                         , ylab="Avg. No. of Steps", main="Average Steps per interval"))
lines(StepsByInterval$steps, col="blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
#The 5-minute interval that, on average, contains the maximum number of steps
UpperLimit <- StepsByInterval$interval[which(StepsByInterval$steps == max(StepsByInterval$steps))]
LowerLimit <- StepsByInterval$interval[as.numeric(UpperLimit)-1]
```

The 5-minute interval that, on average, contains the maximum number of steps is from 830 to 835. It is assumed that the steps recorded in each row were for the preceeding 5-minute period.

##Code to describe and show a strategy for imputing missing data

```r
naRowCount <- sum(!complete.cases(activity))
sum(is.na(activity$steps))
sum(is.na(activity$date))
sum(is.na(activity$interval))
```

From the above code it was determined that all the missing data is from only one variable i.e. "steps". There are 2304 rows with missing data and these rows are updated with average steps per 5-min interval from the corresponding interval.
The Original data is copied to a new dataframe named "ActivityUpdated" and the missing data is replaced with the average steps per interval matched by the interval variable.

```r
activityUpdated <- activity
naIndex <- is.na(activityUpdated$steps)
activityUpdated$steps[which(naIndex)] <- StepsByInterval$steps[match(activityUpdated$interval[which(naIndex)], StepsByInterval$interval)]
```

##Calculating the total number of steps taken in each day

```r
StepsByDay2 <- group_by(activityUpdated, date)
StepsByDay2 <- summarise(StepsByDay2, steps = sum(steps,na.rm = TRUE))
```

##Histogram of the total number of steps taken each day using updated data
Ploting the Histogram of the total number of steps taken each day after missing values are imputed

```r
with(StepsByDay2, barplot(steps, names.arg=date, border = "black", col="Red", xlab="Date"
                         , ylab="Number of Steps", main="Total Steps per day"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
#Mean and median number of steps taken each day with updated data
MeanStepsByDay2 <- mean(StepsByDay2$steps)
MedianStepsByDay2 <- median(StepsByDay2$steps)
```

The new mean and median number of steps per day calculated using the updated data are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup> respectively.

##Determining the day type and adding the new variable to updated data

```r
DayType <- weekdays(activityUpdated$date)
DayType <- ifelse(DayType == "Saturday" | DayType == "Sunday", "Weekend", "Weekday")
DayType <- as.factor(DayType)
activityUpdated <- cbind(activityUpdated, DayType)
#table(activityUpdated$DayType)
```

##Seperating the Weekend and Weekday data and calculating the average steps by interval

```r
WeekdayStepsByInterval <- group_by(subset(activityUpdated, DayType=="Weekday"), interval)
WeekdayStepsByInterval <- summarise(WeekdayStepsByInterval, steps = mean(steps))

WeekendStepsByInterval <- group_by(subset(activityUpdated, DayType=="Weekend"), interval)
WeekendStepsByInterval <- summarise(WeekendStepsByInterval, steps = mean(steps))
```

##Comparing average number of steps taken in each interval across weekdays and weekends
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
par(mfrow = c(2,1), mar = c(4,4,2,1))
with(WeekdayStepsByInterval, {
    plot(interval, steps, type = "n", col="Red", xlab="Time", ylab="Avg. No. of Steps"
         , main="Average Steps per interval on Weekdays")
    lines(steps, col="blue")
})
with(WeekendStepsByInterval, {
    plot(interval, steps, type = "n", col="Red", xlab="Time", ylab="Avg. No. of Steps"
         , main="Average Steps per interval on Weekend")
    lines(steps, col="blue")
})
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

From the panel plot, We can see that the steps looks more distributed during weekend than during weekdays.
