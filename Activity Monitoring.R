# Activity monitoring project 1( Reproducible Research)
# loading the required packages for the project
library(ggplot2)
library(dplyr)
#Read in the data
Activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
head(Activity)
summary(Activity)
# processing the data
# convert date from factor to date
Activity$date <- as.Date(Activity$date)
# removing NA's and cleaning the data
Activity_noNA <- na.omit(Activity)
head(Activity_noNA)

# finding the sum of steps taken each day and storing in a dataframe 
dailysum <- aggregate(Activity_noNA$steps ~ Activity_noNA$date, FUN = sum)
colnames(dailysum) <- c("date","steps")
head(dailysum)

# Plotting a histogram of the total number of steps taken each day versus the date
hist(dailysum$steps, breaks = 20, xlab = "Steps", main = "Total Steps taken per day", col = "blue")
# mean of number of steps taken per day
mean(dailysum$steps)
#median of number of steps taken per day
median(dailysum$steps)

#Time series plot of the average number of steps taken

# calculate the average number of steps taken in each time interval in a day
averageSteps <- aggregate(Activity_noNA$steps, list(interval = as.numeric(Activity_noNA$interval)), FUN = "mean")
colnames(averageSteps) <- c("interval", "meanOfSteps")
head(averageSteps)

# Make a time series line plot for the average number of steps(X axis) Vs time interval(Y-axis)
ggplot(averageSteps, aes(interval, meanOfSteps)) + geom_line(color = "blue", size = 1.0) + 
       labs(x = "Time in 5 min intervals", y = " Average number of steps for each 5 min.interval", 
            title = "Time Series Plot of average steps per interval") 

# The 5 minute interval that on average contains the maximum number of steps
MaximumSteps <- max(averageSteps$meanOfSteps)
averageSteps[averageSteps$meanOfSteps == MaximumSteps, 1]

# 6. Code to describe and show a strategy for imputing missing data

# The number of rows with NA
nrow(Activity[is.na(Activity$steps),])
# There are 2304 rows with steps = NA
# In order to fill in values for missing data we could calculate the mean for the 5 minute interval.
library(plyr)
Activity_noNA$day <- weekdays(as.Date(Activity_noNA$date))

# calculate the average number of steps per 5 minute interval and the day of the week
averageTable <- ddply(Activity_noNA, .(interval,day), summarize, 
                      meanSteps = mean(Activity_noNA$steps))
head(averageTable)
# create data set with NA's for merging with clean data
NAdata <- Activity[is.na(Activity$steps), ]
# substitute NA's in this dataset with the dataset having average number of steps per weekday per interval
newData <- Activity
for(i in 1:nrow(newData)){
 if (is.na(newData$steps[i])){
   newData$steps[i] <- averageSteps[which(newData$interval[i] == averageSteps$interval), ]$meanSteps
 }
   } 
head(newData)

# 7. Histogram of the total number of steps taken each day after missing values are imputed
hist(newData$steps, breaks = 20, xlab = "steps taken per day", 
     main = "Total Steps taken each day after imputing missing values", col= "Yellow")
ggplot(newData, aes(date,steps)) + geom_bar(stat = "identity", colour = "yellow", width = 0.8)
+ facet_grid(. ~ month, scales = "free") + labs( x = "Steps taken per day",
  title = "Total steps taken each day after imputing missing values", y = "Total number of steps")
# Calculating the mean and median after imputing missing values with average for the day for a time interval
newDailySum <- aggregate(newData$steps ~ newData$date, FUN = sum)
colnames(newDailySum) <- c("date","steps")
mean(newDailySum$steps)
median(newDailySum$steps)
# Finding the difference between New and old mean and median 
meanDifference <- mean(newDailySum$steps) - mean(dailysum$steps)
medianDifference <- median(newDailySum$steps) - median(newDailySum$steps)
# There is no difference between the mean of new imputed data and old data
# There is no difference in the median between the old median and the new median

# 8. Panel plot comparing the average number of steps taken per 5-minute interval
#    across weekdays and weekends
# create a factor including weekend and weekdays in the dataset
newData$weekdays <- factor(format(newData$date, "%A"))
levels(newData$weekdays)
levels(newData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
# calculate the average steps across weekdays and weekends
weekdayAverage <- aggregate(newData$steps, list(interval = as.numeric(newData$interval), weekdays = newData$weekdays), FUN = "mean")
names(weekdayAverage)[3] <- "meanSteps"
library(lattice)
xyplot(weekdayAverage$meanSteps ~ weekdayAverage$interval | weekdayAverage$weekdays,
       layout = c(1,2), type = "l", xlab = "interval", ylab = "Number of steps")
