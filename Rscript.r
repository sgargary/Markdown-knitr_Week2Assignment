

---
title: "PA1_template_revised"
author: "Sahar Gargary"
date: "April 22, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


##Loading and preprocessing the data 

```{r Loading and preprocessing the data}
library(dplyr)
library(readr)
library(lattice)

 setwd("C:/2018/Data_Sahar/Courses/5_Reproducible Research/W2_Markdown & knitr/HW")
activity <- read_csv("activity.csv")
activity <- as.data.frame(activity)

```
##What is mean total number of steps taken per day?


```{r What is mean total number of steps taken per day, fig.path = "/figs/"}
stepD <- activity %>%
  group_by(date)  %>%
  summarise(step = mean(as.numeric(steps),na.rm=TRUE))

hist(stepD$step, col = "red")
mean(stepD$step, na.rm = TRUE)
median(stepD$step, na.rm = TRUE)

```

##What is the average daily activity pattern?

```{r What is the average daily activity pattern, fig.path = "/figs/"}
stepI <- activity %>%
  group_by(interval)  %>%
  summarise(step = mean(as.numeric(steps),na.rm=TRUE))
plot(stepI$interval,stepI$step,  xlab = "5-minute interval", ylab = "steps taken", type = "l")

stepI$interval[which.max(stepI$step)]

```
##Imputing missing values

```{r Imputing missing values, fig.path = "/figs/"}
length(which(is.na (activity$steps)))
activityna<- activity %>%
  mutate(steps = ifelse(is.na(steps), mean(activity$steps[activity$interval == interval],na.rm=TRUE), steps ))

hist(activityna$steps, col = "red")
mean(activityna$steps, na.rm = TRUE)
median(activityna$steps, na.rm = TRUE)

summary(activityna)
summary(activity)
```
Do these values differ from the estimates from the first part of the assignment? No
What is the impact of imputing missing data on the estimates of the total daily number of steps?

##Are there differences in activity patterns between weekdays and weekends?

```{r Are there differences in activity patterns between weekdays and weekends, fig.path = "/figs/"}
activitywd <- activityna %>%
  mutate(weekdays = weekdays(activity$date)) %>%
  mutate(wd = ifelse(weekdays %in% c("Saturday","Sunday"), "Weekend","Weekday"))
 
stepwd <- activitywd %>%
  group_by(interval,wd)  %>%
  summarise(step = mean(as.numeric(steps),na.rm=TRUE)) 
  
 # plot(stepwd$interval,stepwd$step,  xlab = "Interval", ylab = "Number of steps", type = "l")
 xyplot(step~interval|wd,stepwd,type='l', layout = c(1,2))
 
 
```
