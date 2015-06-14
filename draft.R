# setwd("/Users/drmoreel/GitRepo/RepData_PeerAssessment1/data/")
# newest RStudio memory issues, errors on unzip on the fly with read.csv 
# http://stackoverflow.com/questions/22643372/embedded-nul-in-string-error-when-importing-huge-csv-with-fread

data <- read.csv(file = "activity.csv", as.is = TRUE)

library(dplyr)
library(lubridate)
library(lattice)

data$date <- ymd(data$date)


## task 1: What is mean total number of steps taken per day?

perdiem <- data %>%
  group_by(date) %>%
  summarise(allsteps = sum(steps)) %>%
  filter(allsteps > 0)

hist(perdiem$allsteps)

# summary(perdiem$allsteps)
median(perdiem$allsteps)
# [1] 10765
mean(perdiem$allsteps)
# [1] 10766.19



## task 2: What is the average daily activity pattern?

perinterval <- data %>%
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE)) 

plot(perinterval, type = "l")


max(perinterval$steps)
# [1] 206.1698
perinterval$interval[max(perinterval$steps)]
# [1] 1705




## task 3: Imputing missing values

dim(data)
# [1] 17568     3
sum(is.na(data$steps))
# [1] 2304

# fill the NA's with the mean for that 5-minute interval

newdata <- data %>%
  filter(is.na(steps) == TRUE) %>%
  select(date, interval)

newdata <- inner_join(x = newdata, y = perinterval, by = "interval")

data <- filter(data, is.na(steps) == FALSE)
data <- rbind(data, newdata)

# verify operation on dataset
sum(is.na(data$steps))
# [1] 0
dim(data)
# [1] 17568     3

# verify changed summary
# first recalculate perdiem with newdata included
median(perdiem$allsteps)
# [1] 10766.19
mean(perdiem$allsteps)
# [1] 10766.19



## task 4: Are there differences in activity patterns between weekdays and weekends?

moredata <- data %>%
  mutate(weekday = wday(date))
moredata$weekday[moredata$weekday == 1] <- "weekend"
moredata$weekday[moredata$weekday == 7] <- "weekend"
moredata$weekday[nchar(moredata$weekday) == 1] <- "weekday"
moredata$weekday <- as.factor(moredata$weekday)

groupedperinterval <- moredata %>%
  group_by(weekday, interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE)) 

xyplot(steps ~ interval | weekday, data = groupedperinterval, type = "l")

