---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

library(ggplot2)

# Downloading the data
setwd("/Users/manaspande/Desktop/RStudio Workspace/RepData_PeerAssessment1-master")
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "/Users/manaspande/Desktop/RStudio Workspace/RepData_PeerAssessment1-master/activity_data.zip")

# Loading and preprocessing the data
unzip("/Users/manaspande/Desktop/RStudio Workspace/RepData_PeerAssessment1-master/activity_data.zip", exdir = "/Users/manaspande/Desktop/RStudio Workspace/RepData_PeerAssessment1-master/")
activity <- read.csv("/Users/manaspande/Desktop/RStudio Workspace/RepData_PeerAssessment1-master/activity.csv")

# Histogram of the total number of steps taken each day
dailySteps <- aggregate(steps~date,activity,sum,na.rm=TRUE)
png("plot1.png")
hist(dailySteps$steps,xlab = "Total number of steps taken per day",main = "Histogram of Daily Steps")
dev.off()

# Mean and Median number of steps taken per day
meanDailySteps <- mean(dailySteps$steps)
medianDailySteps <- median(dailySteps$steps)

# Time series plot of the average number of steps taken per day
intervalSteps <- aggregate(steps~interval,activity,mean,na.rm=TRUE)
png("plot2.png")
plot(steps~interval,intervalSteps,type="l")
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxStepsIndex <- which.max(intervalSteps$steps)
intervalWithMaxSteps <- intervalSteps[maxStepsIndex,]$interval

# Total number of NAs in the dataset
totalNA <- sum(is.na(activity$steps))

# New dataset with NA values filled in with the mean value for that interval
imputedSteps <- ifelse(is.na(activity$steps), round(intervalSteps$steps[match(activity$interval, intervalSteps$interval)],0), activity$steps)
newActivityDataset <- data.frame(steps=imputedSteps,date=activity$date,interval=activity$interval)

# Histogram of the total number of steps taken each day for the new dataset
newDailySteps <- aggregate(steps~date,newActivityDataset,sum)
png("plot3.png")
hist(newDailySteps$steps,xlab = "Total number of steps taken per day",main = "Histogram of Total Daily Steps for the New Dataset")
dev.off()

# Mean and Median number of steps taken per day for the new dataset
newMeanDailySteps <- mean(newDailySteps$steps)
newMedianDailySteps <- median(newDailySteps$steps)

# Imputing missing data does seem to have a small effect on the mean and median values. Mean value went from 10766.19
# to 10765.64 while the median value went from 10765 to 10762

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given
# date is a weekday or weekend day.
newActivityDataset$RealDate <- as.Date(newActivityDataset$date, format = "%Y-%m-%d")
newActivityDataset$Weekday <- weekdays(newActivityDataset$RealDate)
newActivityDataset$DayType <- ifelse(newActivityDataset$Weekday=='Saturday' | newActivityDataset$Weekday=='Sunday', 'Weekend','Weekday')

# Two time series plot of the 5-minute interval (x) and the average number of steps taken averaged across weekdays
# or weekends (y)
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=newActivityDataset,FUN=mean,na.action=na.omit)
StepsPerTimeDT$time <- intervalSteps$interval/100
png("plot4.png")
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="black")+ggtitle("Weekdays v/s Weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
dev.off()

