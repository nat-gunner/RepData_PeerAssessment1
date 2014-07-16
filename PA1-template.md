Reproducible Data - Assignment 1 -- Activity Monitoring Data
========================================================

## Notes before beginning

Knitting this R markdown will require downloading and installing the reshape2 and psych packages.

## Loading and preprocessing the data

This chunk of code will read the activity.csv file in the current working directory and create a data frame with two character columns and one numeric column.  The "interval" column is class character to make it easier to convert to hours and minutes later.


```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", "character"))
```

This next code chunk will take the 5-minute intervals and convert them into strings that are ready to be switched to POSIX.  It stores the POSIX-ready strings in a new column on the right side of the activity data frame.


```r
## This code creates a function to evaluate the value in the interval column and perform formatting operations
## based on the character length of the value.

convertInterval <- function(x){
  df <- data.frame(stringsAsFactors=FALSE)
  for (i in x){
    if (nchar(i) == 2){
      i <- paste(c("00"),i,sep=":")
      df <- rbind(df,i)
      df[,1] <- as.character(df[,1])
    }
    else if (nchar(i) == 1){
      i <- paste("0",i,sep="")
      e <- paste("00",i,sep=":")
      df <- rbind(df,e)
      df[,1] <- as.character(df[,1])
    }
    else if (nchar(i) == 3){
      x <- substr(i, 0, nchar(i)-2)
      hr <- paste("0",x,sep="")
      mins <- substr(i, nchar(i)-1, nchar(i))
      i <- as.character(paste(hr, mins, sep=":"))
      df <- rbind(df,as.character(i))
    }
    else {
      hr <- substr(i, 0, nchar(i)-2)
      mins <- substr(i, nchar(i)-1, nchar(i))
      i <- as.character(paste(hr, mins, sep=":"))
      df <- rbind(df,as.character(i))
    }
  }
  df
}

## This line runs the function above on the interval values in the activity dataset and creates a new column 
## with the formatted times.

activity[,4] <- convertInterval(activity[,3])

##  This line prints out the first few lines of the resulting activity dataset to demonstrate what was done.

head(activity)
```

```
##   steps       date interval X.00.00.
## 1    NA 2012-10-01        0    00:00
## 2    NA 2012-10-01        5    00:05
## 3    NA 2012-10-01       10    00:10
## 4    NA 2012-10-01       15    00:15
## 5    NA 2012-10-01       20    00:20
## 6    NA 2012-10-01       25    00:25
```



To get the date info into POSIX format so we can perform some of the subsetting and data calculations required for the assignment, we'll need to add the time strings to the date that we already have.  The first step is to add the number of minutes from the fourth column we just created to the "date" column.


```r
## Paste the formatted intervals to the "date" column

activity[,2] <- paste(activity[,2], activity[,4], sep=" ")
head(activity)
```

```
##   steps             date interval X.00.00.
## 1    NA 2012-10-01 00:00        0    00:00
## 2    NA 2012-10-01 00:05        5    00:05
## 3    NA 2012-10-01 00:10       10    00:10
## 4    NA 2012-10-01 00:15       15    00:15
## 5    NA 2012-10-01 00:20       20    00:20
## 6    NA 2012-10-01 00:25       25    00:25
```

Now that we have the properly formatted time in the same colum as the date, we'll use strptime to convert the date to POSIX.


```r
## Next, convert the "date" column to POSIX

activity[,2] <- as.POSIXct(strptime(activity[,2], format="%Y-%m-%d %H:%M"))
```

## Finding the mean total number of steps taken per day

Now that the data is cleaned up somewhat, we're ready to perform some analysis.  This code chunk will look at all the complete cases and get the total number of steps for each day.  It then creates a histogram of the number of daily steps and prints the mean and median number of steps.


```r
## Remove all non-complete cases

activity2 <- activity[complete.cases(activity),]

## Use tapply to get the sum for each date

stepSum <- tapply(activity2$steps,format(activity2$date, '%Y-%m-%d'),sum)

## Reformat using melt

library(reshape2)

stepSum2 <- melt(stepSum, id.vars=c("x", "row.names"))

## Make a histogram of the number of daily steps

hist(stepSum2$value, main="Total # of Steps", xlab="Number of daily steps")
```

![plot of chunk daily_sum](figure/daily_sum.png) 

```r
## Use describe() function to get some summary statistics

library(psych)

describe(stepSum2$value)
```

```
##   vars  n  mean   sd median trimmed  mad min   max range skew kurtosis
## 1    1 53 10766 4269  10765   10940 3140  41 21194 21153 -0.3     0.59
##      se
## 1 586.4
```

```r
## Print the mean

print(mean(stepSum2$value))
```

```
## [1] 10766
```

```r
## Print the median

print(median(stepSum2$value))
```

```
## [1] 10765
```

## Plotting the average daily activity pattern

This next code chunk gets the average number of steps for each 5-minute interval then creates a line graph of the data.

It then prints out the 5-minute interval that contains the most steps on average.



```r
## First, use tapply to get the average number of steps for each interval

intervalMean <- tapply(activity2$steps,activity2$interval,mean)

## Then reformat the result with melt

intervalMean2 <- melt(intervalMean, id.vars=c("row.names", "x"))

## Re-order the results to prepare the data for plotting

intervalMean3 <- intervalMean2[order(as.numeric(intervalMean2$Var1)),]

## Plot the data

plot(intervalMean3, type="l", xlab="Interval", ylab="Avg # of steps")
```

![plot of chunk interval_avg](figure/interval_avg.png) 

```r
## Print out the interval with the highest average number of steps

maxSteps <- max(intervalMean3$value)
maxInt <- intervalMean3$Var1[intervalMean3$value == maxSteps]
maxInt
```

```
## [1] 835
```

## Inputting missing values

The next code chunk will replace the NA values in the steps column with the average step count for the corresponding interval.  


```r
## Create a list of all the interval values that have NAs associated with them

int <- as.integer(activity$interval[is.na(activity$steps)])

## Set up a vector to hold the average values

val=c()

## Loop through the values in int and grab the corresponding average step values

for (i in int) {
  val <- c(val, intervalMean3$value[intervalMean3$Var1 == i])
  }

## Replace the NAs with the average step values

activity$steps[is.na(activity$steps)] <- val

## Get the total number of steps taken for each day

comSum <- tapply(activity$steps,format(activity$date, '%Y-%m-%d'),sum)

library(reshape2)

comSum2 <- melt(comSum, id.vars=c("x", "row.names"))

hist(comSum2$value, main="Total # of Steps", xlab="Number of daily steps")
```

![plot of chunk replace_NA](figure/replace_NA.png) 

```r
## Print the mean

print(mean(comSum2$value))
```

```
## [1] 10766
```

```r
## Print the median

print(median(comSum2$value))
```

```
## [1] 10766
```

## Comparing weekends and weekdays

This final chunk of code will create a new factor variable in the activity data set with two levels: "weekend" and "weekday".  It will then evaluate the dates in the data set to determine whether a date is a weekend or a weekday


```r
activity$date <- as.Date(activity$date)

dayList <- c()

for (i in weekdays(activity$date)){
    if (i %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")){
      dayList <- c(dayList, "weekday")
    }
    else{
      dayList <- c(dayList, "weekend")
  }
}

activity[,5] <- dayList

weekend <- activity[activity$V5 == "weekend",]
weekday <- activity[activity$V5 == "weekday",]

wkdMean <- tapply(weekend$steps,weekend$interval,mean)
wkdMean2 <- melt(wkdMean, id.vars=c("row.names", "x"))
wkdMean3 <- wkdMean2[order(as.numeric(wkdMean2$Var1)),]

wdyMean <- tapply(weekday$steps,weekday$interval,mean)
wdyMean2 <- melt(wdyMean, id.vars=c("row.names", "x"))
wdyMean3 <- wdyMean2[order(as.numeric(wdyMean2$Var1)),]

par(mfrow=c(2,1))

plot(wdyMean3, type="l", main="Weekday", ylab="Average number of steps", xlab="Time interal")
plot(wkdMean3, type="l", main="Weekend", ylab="Average number of steps", xlab="Time interal")
```

![plot of chunk compare_times](figure/compare_times.png) 

```r
par(mfrow=c(1,1))
```

As you can see, the activity patterns for weekdays and weekends are different. 
