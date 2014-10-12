# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
library(ggplot2)
#Read Activity file
steps <-read.csv(file.choose())
steps$date <- as.Date(steps$date)
```

## What is mean total number of steps taken per day?


```r
output<-aggregate(steps$steps,by=list(steps$date),FUN=sum, na.rm=TRUE)
output<-cbind(output,as.Date(output[,1]))
colnames(output)<-c("Date","TotalSteps","Day")

meanout<- aggregate(steps$steps,by=list(steps$date),FUN=mean, na.rm=TRUE)
colnames(meanout)<-c("Date","AvgSteps")

medianout<- aggregate(steps$steps,by=list(steps$date),FUN=median, na.rm=TRUE)
colnames(medianout)<-c("Date","MedianSteps")

# Print Plot of Total Steps per Day

a<-ggplot(output,aes(x=Day,y=TotalSteps))+geom_bar(stat="identity")
print(a)
```

![plot of chunk unnamed-chunk-2](./PA1_Template_files/figure-html/unnamed-chunk-2.png) 


## What is the average daily activity pattern?

```r
intervaldata<-aggregate(steps$steps,by=list(steps$interval),FUN=mean, na.rm=TRUE)
#Print Average Steps by Interval
colnames(intervaldata)<-c("Interval","AverageSteps")

b<-qplot(Interval,AverageSteps,data=intervaldata,geom=c("line"))
print(b)
```

![plot of chunk unnamed-chunk-3](./PA1_Template_files/figure-html/unnamed-chunk-3.png) 

```r
# Mean of TotalSteps
mean(output$TotalSteps)
```

```
## [1] 9354
```

```r
#Median of Total Steps
median(output$TotalSteps)
```

```
## [1] 10395
```

```r
#Interval when Max Steps was observed
maxinterval<-intervaldata[intervaldata$AverageSteps == max(intervaldata$AverageSteps),1]

maxinterval
```

```
## [1] 835
```

## Imputing missing values

```r
numrowswithna<-nrow(steps[is.na(steps$steps),])
nstep<-steps

for ( i in 1:nrow(steps)){
  if (is.na(steps[i,1])){
    tmp<-intervaldata[intervaldata$Interval == steps[i,3],2]
    if (is.na(tmp)){ steps[i,1]<-0}
    else{
      steps[i,1]<-tmp
    }
  }
}
newout<-aggregate(steps$steps,by=list(steps$date),FUN=sum, na.rm=TRUE)
newout<-cbind(newout,as.Date(newout[,1]))
colnames(newout)<-c("Date","TotalSteps","Day")

newmeanout<- aggregate(steps$steps,by=list(steps$date),FUN=mean, na.rm=TRUE)
colnames(newmeanout)<-c("Date","AvgSteps")

newmedianout<- aggregate(steps$steps,by=list(steps$date),FUN=median, na.rm=TRUE)
colnames(newmedianout)<-c("Date","MedianSteps")

a<-ggplot(newout,aes(x=Day,y=TotalSteps))+geom_bar(stat="identity")
print(a)
```

![plot of chunk unnamed-chunk-4](./PA1_Template_files/figure-html/unnamed-chunk-4.png) 

## Are there differences in activity patterns between weekdays and weekends?

```r
nexsteps <- steps
nexsteps$dow<-0
for ( i in 1:nrow(nexsteps)){
  if ( weekdays(nexsteps$date[i]) %in% c("Saturday","Sunday")){
    nexsteps$dow[i]<-1
  }
  
}
awd<-subset(nexsteps,nexsteps$dow==0,select=c("steps","date","interval"))
wd<-aggregate(awd$steps,by=list(awd$interval),FUN=mean, na.rm=TRUE)
wd$weekday<-"weekday"
colnames(wd)<-c("Interval","AverageSteps","Day")
bwe<-subset(nexsteps,nexsteps$dow==1,select=c("steps","date","interval"))
we<-aggregate(bwe$steps,by=list(bwe$interval),FUN=mean, na.rm=TRUE)
we$weekday<-"weekend"
colnames(we)<-c("Interval","AverageSteps","Day")

all<- rbind(wd,we)

d<-qplot(Interval,AverageSteps,data=all,facets=Day~.,geom=c("line"))
d<-d+facet_wrap(~Day,ncol=1)
print(d)
```

![Weekdays Vs Weekends](./PA1_Template_files/figure-html/unnamed-chunk-5.png) 

###Interesting project - I am sure there are shorter and more efficient ways to code!!!
