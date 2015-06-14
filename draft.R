# setwd("/Users/drmoreel/GitRepo/RepData_PeerAssessment1/data/")
# newest RStudio memory issues, errors on unzip on the fly with read.csv 
# http://stackoverflow.com/questions/22643372/embedded-nul-in-string-error-when-importing-huge-csv-with-fread

data <- read.csv(file = "activity.csv", as.is = TRUE)

library(dplyr)
library(lubridate)

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

