# Reproducible Research: Peer Assessment 1
----  

Getting ready . . .
 

```r
library(markdown)
library(knitr)
library(plyr)
library(stringr)
library(ggplot2)

setwd("~/RepData_PeerAssessment1")
temp <- unzip("activity.zip")
```
----   

## Loading and preprocessing the data  

* 1. Read in the data, e.g.* `read.csv()`  

**Rubric:  Is there code to read the data?**   


```r
df <- read.csv(file = "activity.csv"
               , head=TRUE
               , na.strings = "NA"
               )
```

* 2. Process and transform the data.*  
**Rubric:  Is there code to process the data?**  
I thought it would be nice to have a datetime column, good practice working with dates.  

```r
df$datetime<-strptime(paste(df$date
                                , " "
                                 , str_sub(paste("0",(60*df$interval%/%100)/60,sep=""),-2,-1)
                                 , ":"           
                                 , str_sub(paste("0",df$interval%%100,sep=""),-2,-1)
                                , ":00"
                                 , sep=""
                           )
                     , "%Y-%m-%d %H:%M:%S"
                     )
df$datetime<- as.POSIXct(strptime(as.character(df$datetime)
                                  , "%Y-%m-%d %H:%M:%S"
                                  )
                         )
```

* Make a weekend factor column (I'm assuming R is set up for English language here)*  

**Rubric:  Is there code to process the data?**  


```r
df$weekend <- as.factor(
                        weekdays(df$datetime)=="Saturday"| 
                        weekdays(df$datetime)=="Sunday"
                        )
df$weekend <- ordered(df$weekend,
                     levels = c(TRUE,FALSE),
                     labels = c("Weekend", "Weekday"))
```

Now let's see what we've got . . .  


```r
str(df)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ datetime: POSIXct, format: "2012-10-01 00:00:00" "2012-10-01 00:05:00" ...
##  $ weekend : Ord.factor w/ 2 levels "Weekend"<"Weekday": 2 2 2 2 2 2 2 2 2 2 ...
```

```r
head(df)
```

```
##   steps       date interval            datetime weekend
## 1    NA 2012-10-01        0 2012-10-01 00:00:00 Weekday
## 2    NA 2012-10-01        5 2012-10-01 00:05:00 Weekday
## 3    NA 2012-10-01       10 2012-10-01 00:10:00 Weekday
## 4    NA 2012-10-01       15 2012-10-01 00:15:00 Weekday
## 5    NA 2012-10-01       20 2012-10-01 00:20:00 Weekday
## 6    NA 2012-10-01       25 2012-10-01 00:25:00 Weekday
```

Looks good - ready to start analysis!
----  

## What is mean total number of steps taken per day?  
*For this part of the assignment, you can ignore the missing values in the dataset.*  

### 1. Make a histogram of the total number of steps taken each day
**Rubric:  Is there a histogram and code for the histogram?**  


```r
StepsPerDay <-aggregate(steps~date, data=df, sum, na.rm=TRUE)
hist(StepsPerDay$steps
     , xlab="Steps Per Day"
     , main= "Missing Values Removed"
     , cex.axis=.8
     , labels=TRUE
     , ylim=c(0, 40)
     , col="light blue"
     )
grid(NA,NULL,lwd = 2) 
box()
```

![plot of chunk makehist1](figure/makehist1.png) 
  
### 2. Calculate and report the mean and median total number of steps taken per day  
**Rubric:  Is there code to process the data?**  

```r
StepsPerDay <-aggregate(steps~date, data=df, sum, na.rm=TRUE)
head(StepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### Report the mean and median: 
The mean is 1.0766 &times; 10<sup>4</sup> and the median is 10765.
**Rubric:  Are both the mean and median number of steps taken each day reported?** 
NOTE: They are calculated inline in the statement above.

----  

## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

**Rubric: Is there a time series plot of the average number of steps taken (averaged across all days) versus the 5-minute intervals?**  

```r
AvgStepsPerInterval <- aggregate(steps~interval, data=df,mean,na.rm=TRUE)

MaxPt <-AvgStepsPerInterval[AvgStepsPerInterval$steps==max(AvgStepsPerInterval$steps),]

plot(  AvgStepsPerInterval$interval
     , AvgStepsPerInterval$steps
     , type='l' 
     , xlab="Interval"
     , ylab= "Average # Steps"
     , col="blue"
    )
text(  MaxPt$interval
     , MaxPt$steps
     , paste("Max =", trunc(MaxPt$steps,0), "steps @ interval", MaxPt$interval)
     , cex = .8
     , pos = 4
     , col = "red"
    )
grid(NULL,NULL,lwd = 2) 
```

![plot of chunk maketsplot](figure/maketsplot.png) 
  
### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

**Rubric: Does the report give the 5-minute interval that, on average, contains the maximum number of steps?**  

Here's the answer: The 5-minute interval containing the maximum number of steps is 835 and the maximum # of steps is 206.1698, as seen below


```r
MaxPt
```

```
##     interval steps
## 104      835 206.2
```

----  

## Imputing missing values
*Note that there are a number of days/intervals where there are missing values (coded as NA).* 

### 1. Calculate and report the total number of missing values in the dataset  

The NA count can be seen here . . .  
**Rubric: Does the report show all of the R code needed to reproduce the results (numbers, plots, etc.) in the report?**  


```r
summary(df$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. 
**Rubric: Does the report *describe* and show with code a strategy for imputing missing data?**  

Describe:  My strategy is to replace each missing value with the average # of steps during the same interval from the non-missing rows.  Please see the code chunk below for the calculation.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
 
The new dataset will be named df.imp as shown in the code chunk below.

NOTE:  I found this code [here](http://stackoverflow.com/questions/9322773/how-to-replace-na-with-mean-by-subset-in-r-impute-with-plyr).   
**Rubric: Does the report describe and *show with code* a strategy for imputing missing data?**  


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df.imp <- ddply(df, ~ interval, transform, steps = impute.mean(steps))  
df.imp<-df.imp[order(df.imp$date,df.imp$interval), ] 
```

Now compare the two data frames.  df has NA's and df.imp does not.  


```r
summary(df.imp$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0    37.4    27.0   806.0
```

```r
summary(df$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

Result:  The mean and median were unaffected but the 3rd Quartile increased by 15.  Not at all surprising given the imputation calculation as interval averages.

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  
### Do these values differ from the estimates from the first part of the assignment? 
### What is the impact of imputing missing data on the estimates of the total daily number of steps?  

**Rubric: Does the report contain a histogram of the total number of steps taken each day after missing values were imputed?**  


```r
StepsPerDay.Imp <-aggregate(steps~date, data=df.imp, sum)
hist(  StepsPerDay.Imp$steps
     , xlab = "Steps Per Day"
     , main = "Missing Values Imputed"
     , cex.axis = .8
     , labels = TRUE
     , ylim = c(0, 40)
     , col = "light blue"
     )
grid(NA,NULL,lwd = 2) 
box()
```

![plot of chunk hist2](figure/hist2.png) 

The histogram is only slightly changed compared to the first one.  

Just for further comparison, I also compared the statistics and did another time series plot.


```r
c(sum(StepsPerDay$steps,na.rm=TRUE),sum(StepsPerDay.Imp$steps,na.rm=TRUE))
```

```
## [1] 570608 656738
```

```r
c(mean(StepsPerDay$steps,na.rm=TRUE),mean(StepsPerDay.Imp$steps,na.rm=TRUE))
```

```
## [1] 10766 10766
```

```r
c(median(StepsPerDay$steps,na.rm=TRUE),median(StepsPerDay.Imp$steps,na.rm=TRUE))
```

```
## [1] 10765 10766
```

The mean and median were hardly affected at all, but the sum increased by over 86K steps (about 8 days' worth).  

## What is the impact of imputing missing data on the estimates of the total daily number of steps?  

Now look at the new time series plot . . .


```r
AvgStepsPerInterval.Imp <- aggregate(steps~interval, data=df.imp,mean)

MaxPt.Imp <-AvgStepsPerInterval.Imp[AvgStepsPerInterval.Imp$steps==max(AvgStepsPerInterval.Imp$steps),]

plot(AvgStepsPerInterval.Imp$interval,AvgStepsPerInterval.Imp$steps
     ,type='l' ,xlab="Interval",main="Avg Steps per Interval, Missing Values Imputed"
     ,ylab= "Average # Steps", col="blue")
text(MaxPt.Imp$interval,MaxPt.Imp$steps
     , paste("Max ="
             ,trunc(MaxPt.Imp$steps,0)
             ,"steps @ interval"
             , MaxPt$interval
     ), cex=.8
     , pos=4
     , col="red"
    )
grid(NULL,NULL,lwd = 2) 
```

![plot of chunk tsplot2](figure/tsplot2.png) 
  
2. Now which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
**Rubric:  What was the impact of imputing missing values?**

```r
MaxPt.Imp
```

```
##     interval steps
## 104      835 206.2
```

The maximum point stayed the same as before.  Not surprising given the imputation method I chose.  

 ----  
 
## Are there differences in activity patterns between weekdays and weekends?  

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. 

**NOTE TO ASSESSOR: The weekend column was created during the pre-processing phase at the top of this page.**  

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

**Rubric: Does the report contain a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends?**  


```r
AvgStepsPerIntervalWkend.Imp <- aggregate(steps~interval+weekend, data=df.imp,mean)
sp <- ggplot(AvgStepsPerIntervalWkend.Imp, aes(x=interval, y=steps)) + geom_line()+ ylab("Number of Steps")
sp + facet_wrap(~weekend, ncol=1)
```

![plot of chunk panelplot](figure/panelplot.png) 


