---
title: "Course Project 1"
author: "ahmedadnane.boutahar@gmail.com"
date: "20 mars 2019"
output:
  html_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Description of course
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
## Calculate the total number of steps taken per day
```{r}
url<-'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
zipfile<-'repdata_data_activity.zip'
if(!file.exists(zipfile))
{  
  download.file(url,zipfile,mode="wb")
  unzip(zipfile)
}
activity_data<-read.table("activity.csv",head=TRUE,sep = ",")
aggregated_steps <- tapply(activity_data$steps,activity_data$date,sum,na.rm=TRUE)
hist(aggregated_steps  ,col = "red",main = "Total steps per day")
#Calculate and report the mean and median of the total number of steps taken per day
mean(aggregated_steps)
median(aggregated_steps)

```

##Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
aggregated_steps_interval<-tapply(activity_data$steps,activity_data$interval,mean,na.rm=TRUE)
plot(row.names(aggregated_steps_interval),aggregated_steps_interval,type="l",xlab="Intervals",ylab="Average  Steps",main="Average of Steps VS Intervals")
```

## maximum number of steps

```{r}
max_step<-max(aggregated_steps_interval)
```

## locate 5-minute interval matching with the maximum number of steps

```{r}
aggregated_steps_interval[match(max_step,aggregated_steps_interval)]
```

##Imputing missing values Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 

```{r}
sum(is.na(activity_data))
```

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
activity_data_2<-activity_data

activity_data_2[is.na(activity_data_2),1]<-aggregated_steps_interval
```


##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}

aggregated_steps_2 <- tapply(activity_data_2$steps,activity_data_2$date,sum,na.rm=TRUE)

hist(aggregated_steps_2  ,col = "red",main = "Total steps per day")
#Calculate and report the mean and median of the total number of steps taken per day

mean(aggregated_steps_2)
median(aggregated_steps_2)
```

##Imputing the missing data give to the second histogram,  normal distribution curve

##The mean and meadian for the total steps per day were both 10766.19



##For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
we<-weekdays(as.Date(activity_data$date))
we<-ifelse (we =="samedi" | we =="dimanche","weekend","weekday")
activity_data_w<-cbind(activity_data,we)
aggregated_steps_interval_w<-tapply(activity_data_w$steps,list(activity_data_w$interval,activity_data_w$we),mean,na.rm=TRUE)
library(reshape2)
aggregated_steps_interval_w<-melt(aggregated_steps_interval_w)
colnames(aggregated_steps_interval_w)<-c("interval","day_type","steps")

library(lattice)
xyplot(aggregated_steps_interval_w$steps ~ aggregated_steps_interval_w$interval | aggregated_steps_interval_w$day_type, layout=c(1,2),type="l",xlab="Interval",ylab="Number of Steps")


```

