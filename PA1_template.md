# Reproducible Research: Peer Assessment 1
dar7yl  
Monday, February 09, 2015  
### Project Instructions
* [Project Instructions on website](https://class.coursera.org/repdata-011/human_grading/view/courses/973512/assessments/3/submissions)
* [local Copy](RepData_PeerAssessment1/doc/instructions.pdf)

## Loading and preprocessing the data
The data is from a personal activity monitoring device that collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

* The data for this assignment can be downloaded from the course web site:
		[Activity monitoring data ](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) - 52K

```r
load_data <- function()
{
	library(downloader)
	
	fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
	zipfile <- file.path("data", "repdata_data_activity.zip")
	
	print(paste("Downloading files from '",fileurl,"' to '",zipfile,"'",sep="") )
	
	download.file(fileurl, zipfile, quiet = FALSE, mode = "wb", cacheOK = TRUE )

	print("Unzipping")
	unzip(zipfile, junkpaths=TRUE, exdir="data", unzip="internal", setTimes=FALSE)
	print(paste("file is now in", zipfile), sep=" ")

	#we'll go right ahead and read the data and put it in the global environment
	activity_Classes <- c( "numeric", "Date", "numeric")

	activity <- read.csv(zipfile, colClasses= activity_Classes)
	assign("activity", activity, envir = .GlobalEnv,) 
}
```
## What is mean total number of steps taken per day?

```r
total_steps<-tapply(activity$steps,activity$date,sum)
summary(total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```

```r
mean(total_steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
plot(total_steps)
```

![](PA1_template_files/figure-html/mean_total_steps-1.png) 



## What is the average daily activity pattern?

```r
means<-aggregate( activity$steps, by=list(activity$date), 
						FUN=mean, na.action = na.omit)
summary(means)
```

```
##     Group.1                 x          
##  Min.   :2012-10-01   Min.   : 0.1424  
##  1st Qu.:2012-10-16   1st Qu.:30.6979  
##  Median :2012-10-31   Median :37.3785  
##  Mean   :2012-10-31   Mean   :37.3826  
##  3rd Qu.:2012-11-15   3rd Qu.:46.1597  
##  Max.   :2012-11-30   Max.   :73.5903  
##                       NA's   :8
```

```r
tmean<-mean(means$x, na.rm=TRUE)
tmean
```

```
## [1] 37.3826
```

```r
means.interval<-aggregate( activity$steps, by=list(activity$interval), 
						FUN=mean, na.action = na.omit)
summary(means.interval)
```

```
##     Group.1             x      
##  Min.   :   0.0   Min.   : NA  
##  1st Qu.: 588.8   1st Qu.: NA  
##  Median :1177.5   Median : NA  
##  Mean   :1177.5   Mean   :NaN  
##  3rd Qu.:1766.2   3rd Qu.: NA  
##  Max.   :2355.0   Max.   : NA  
##                   NA's   :288
```

```r
intervals<-tapply(activity$steps,activity$interval,sum)
```


## Imputing missing values

```r
summary(is.na(activity$steps))
```

```
##    Mode   FALSE    TRUE    NA's 
## logical   15264    2304       0
```



## Are there differences in activity patterns between weekdays and weekends?
