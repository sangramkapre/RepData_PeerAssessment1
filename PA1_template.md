# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Following code loads data from **activity.csv** file and saves it in *activity_data* variable.


```r
# read data from activity.csv file
activity_data <- read.csv("activity.csv")
# convert date from Factor to Date format
activity_data$date <- as.Date(as.character(activity_data$date), format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

Following code is for computing number of steps taken per day.


```r
# calculate total number of steps taken per day
steps_per_day <- tapply(activity_data$steps, factor(activity_data$date), FUN = mean)
# plot histogram of steps taken per day
hist(steps_per_day, main = "Steps taken per day", xlab = "Steps per day")
```

![plot of chunk steps_per_day](./PA1_template_files/figure-html/steps_per_day.png) 

```r
# compute mean of steps taken per day
mean_steps_per_day <- mean(steps_per_day, na.rm = TRUE)
# compute median of steps taken per day
median_steps_per_day <- median(steps_per_day, na.rm = TRUE)
```

The mean of number of steps taken per day is: 37.3826.  
The median of number of steps taken per day is: 37.3785.

## What is the average daily activity pattern?

Following code creates a time-series plot of average number of steps taken per day.


```r
# calculate average steps per interval over all days
steps_averaged_across_all_days <- tapply(activity_data$steps, factor(activity_data$interval), function(x) mean(x, na.rm = TRUE))
# plot time series data of average_steps_per_day vs interval
plot(unique(activity_data$interval), steps_averaged_across_all_days, type = "l", main = "Steps per day vs Intervals", xlab = "Time Intervals", ylab = "Average No. of steps (across all days)")
```

![plot of chunk timeseries_of_average_across_all_days](./PA1_template_files/figure-html/timeseries_of_average_across_all_days.png) 


## Imputing missing values

Following code calculates the number of rows in the dataset containing missing values.


```r
# calculate total number of missing values (NAs) in the dataset
no_of_rows_with_missing_values <- length(complete.cases(activity_data)[complete.cases(activity_data) == FALSE])
```

The total number of missing values in the dataset (i.e. the total number of rows with NAs) : 2304.  

Lets replace all the missing values in steps column with the mean of overall steps values.  
Create a new dataset called updated_activity_data from activity_data with NAs replaced.


```r
# replace NAs in steps column with mean of steps
intervalwise_mean_steps_per_day <- tapply(activity_data$steps, activity_data$interval, function(x) mean(x, na.rm = TRUE))
updated_activity_data <- activity_data
updated_activity_data <- within(updated_activity_data, steps[is.na(steps)] <- intervalwise_mean_steps_per_day[as.character(interval[is.na(steps)])])
```

Lets now create a histogram for steps_per_day in updated dataset.  

```r
# calculate total number of steps taken per day for updated dataset
updated_steps_per_day <- tapply(updated_activity_data$steps, factor(updated_activity_data$date), FUN = mean)
# plot histogram of steps taken per day for updated dataset
hist(updated_steps_per_day, main = "Steps taken per day (after removing NAs)", xlab = "Steps per day")
```

![plot of chunk updated_steps_per_day](./PA1_template_files/figure-html/updated_steps_per_day.png) 

```r
# compute mean of steps taken per day in updated dataset
updated_mean_steps_per_day <- mean(updated_steps_per_day)
# compute median of steps taken per day in updated dataset
updated_median_steps_per_day <- median(updated_steps_per_day)
```

The mean of number of steps taken per day in updated dataset is: 37.3826.  
The median of number of steps taken per day in updated dataset is: 37.3826.

These values do not differ from the same from original dataset as we have replaced missing values for steps column with the **mean of entire steps column**.  

## Are there differences in activity patterns between weekdays and weekends?

Following code calculates weekdays and weekends and then splits the data accordingly.  
Then average number of steps across both weekdays and weekends are calculated separately and plotted.


```r
# calculate weekdays and weekends
isWeekday <- (weekdays(activity_data$date) != "Saturday" & weekdays(activity_data$date) != "Sunday")
isWeekday[isWeekday == TRUE] <- "weekday"
isWeekday[isWeekday == FALSE] <- "weekend"
activity_data$isWeekday <- isWeekday
weekdays_activity_data <- activity_data[activity_data$isWeekday == "weekday", ]
weekends_activity_data <- activity_data[activity_data$isWeekday == "weekend", ]
# calculate average steps across weekdays and weekends
steps_averaged_across_all_weekdays <- tapply(weekdays_activity_data$steps, factor(weekdays_activity_data$interval), function(x) mean(x, na.rm = TRUE))
steps_averaged_across_all_weekends <- tapply(weekends_activity_data$steps, factor(weekends_activity_data$interval), function(x) mean(x, na.rm = TRUE))
# plot time series data of average_steps_per_day vs interval for weekdays and weekends
par(mfrow = c(1, 2))
plot(unique(weekdays_activity_data$interval), steps_averaged_across_all_weekdays, type = "l", xlab = "Time Interval", ylab = "Average steps taken across all weekdays", main = "on Weekdays")
plot(unique(weekends_activity_data$interval), steps_averaged_across_all_weekends, type = "l", xlab = "Time Interval", ylab = "Average steps taken across all weekends", main = "on Weekends")
```

![plot of chunk weekdays_vs_weekends](./PA1_template_files/figure-html/weekdays_vs_weekends.png) 
