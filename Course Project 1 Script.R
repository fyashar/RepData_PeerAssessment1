library(dplyr)
library(lubridate)
setwd(dir = '~/R Projects/Reproducible Research/Course Project 1/')
Raw.Data <- read.csv(file = 'activity.csv', header = TRUE)

Tidy.Data <-    Raw.Data %>% 
                mutate(date = ymd(date), time = paste(date ,interval%/%100, interval%%100, sep = " ") %>% ymd_hm()) %>% 
                tbl_df()

## Calculating the total number of steps taken per day
Daily.Total <-  Tidy.Data %>% 
                group_by(date) %>% 
                summarise('daily total' = sum(steps))

hist(Daily.Total$`daily total`)

mean(Daily.Total$`daily total`, na.rm = TRUE)
median(Daily.Total$`daily total`, na.rm = TRUE)

## Calculating the average number of steps taken, averaged across all days.
Average.Daily <-        Tidy.Data %>% 
                        group_by(interval) %>%
                        summarise('average across all days' = mean(steps, na.rm = TRUE))

plot(Average.Daily, type = "l")
## Finding the 5-minute interval which , on average, contains the maximum number of steps
Average.Daily %>% filter(`average across all days` == max(`average across all days`))

## Calculating the total number of missing values in the dataset
Good <- complete.cases(Tidy.Data)
nrow(Tidy.Data[!Good,])
## Devising a strategy for filling in all of the missing values in the dataset
Five.Min.Average <- mean(Tidy.Data$steps, na.rm = TRUE)

for (i in seq_len(nrow(Tidy.Data))) {
        if (is.na(Tidy.Data$steps[i])) {
                Tidy.Data$steps2[i] <- Five.Min.Average
        }
        else {
                Tidy.Data$steps2[i] <- Tidy.Data$steps[i]
        }
        
}

## Creating a new dataset that is equal to the original dataset but with the missing data filled in
Tidy.Data.NA.Filled <- Tidy.Data[,c(5,2,3,4)]
## Calculating the total number of steps taken per day
Daily.Total.2 <-        Tidy.Data.NA.Filled %>% 
                        group_by(date) %>% 
                        summarise('daily total' = sum(steps2))

hist(Daily.Total.2$`daily total`)

mean(Daily.Total.2$`daily total`, na.rm = TRUE)
median(Daily.Total.2$`daily total`, na.rm = TRUE)

## Define a function to determine whether a given date is a weekday or weekend day.
library(timeDate)
f <- function(x) {
        if (isWeekday(x)) {
                x <- c("weekday")
        }
        else {
                x <- c("weekend")
        }
}
## Applying the funtion to the dataset to create a new factor variable with two levels - "weekday" and "weekend"
Tidy.Data.NA.Filled.2 <- Tidy.Data.NA.Filled %>% mutate(weekday = as.factor(sapply(date, f)))

Average.weekdays <-     Tidy.Data.NA.Filled.2 %>% 
                        group_by(interval) %>% 
                        filter(weekday == "weekday") %>% 
                        summarise('average across weekdays' = mean(steps2, na.rm = TRUE))
Average.weekends <-     Tidy.Data.NA.Filled.2 %>% 
                        group_by(interval) %>% 
                        filter(weekday == "weekend") %>% 
                        summarise('average across weekends' = mean(steps2, na.rm = TRUE))
## Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
par(mfcol = c(2,1))
plot(Average.weekends, type = "l", main = "weekends")
plot(Average.weekdays, type = "l", main = "weekdays")

