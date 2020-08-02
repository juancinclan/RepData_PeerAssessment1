---
title: "PA1_template"
author: "juancinclan"
date: "1/8/2020"
output: html_document
---
##Upload to R
```{r}
library(markdown)
library(knitr)
opts_chunk$set(echo=TRUE)
setwd("C:/Users/juanc/OneDrive/Escritorio")
activity<-read.csv("./activity.csv")

```

##Histogram, total numbers of steps
```{r}
totalSteps<-tapply(activity$steps,activity$date,sum)
stepsDates<-names(totalSteps)
totalSteps<-as.data.frame(totalSteps)
library(ggplot2)
ggplot(totalSteps, aes(totalSteps))+geom_histogram()+labs(title = "Total steps") + labs(x = "Steps")

dev.copy(png, file = "HistogramNA.png", width=480, height=480) 
dev.off() 

```

##mean and median
```{r }
m<-mean(totalSteps$totalSteps, na.rm=TRUE)
med<-median(totalSteps$totalSteps, na.rm=TRUE)
```
*Mean=`r m`
*Median=`r med`

##Time series, average of number of steps taken per interval
```{r}

new<-activity
new<-new[-c(2)]
library(reshape2)
meltdata<-melt(new, id=c( "interval"))
Final<-dcast(meltdata,interval~variable, mean,na.rm=TRUE)

ggplot(Final, aes(y=Final$steps ,x=Final$interval, group=1))+geom_line()+geom_point()+labs(title = "Time Series Steps") + labs(x = "Interval")+labs(y="Steps")

dev.copy(png, file = "TimeSeriesNA.png", width=480, height=480) 
dev.off() 


```
##The 5-minute interval that, on average that contains the maximum number of steps
```{r}
maximum<-activity$interval[which(activity$steps==max(activity$steps, na.rm=TRUE))]
```
*Maximum steps interval=`r maximum`

##Number of NA's
```{r}
summary(activity$steps)
```

##Impute missing values
```{r}
## Impute the NA values with the mean of the 5 minute interval
nonNA<-activity
c<-which(is.na(nonNA$steps)==TRUE)
nonNA$steps[c]<-mean(activity$steps, na.rm=TRUE) 
