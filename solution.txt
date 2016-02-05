library(dplyr)
library(ggplot2)

set.seed(42)

data<-read.csv('activity.csv');
by_date <- group_by(data, date);
steps_per_day<-summarize(by_date, sum(steps));
names(steps_per_day)<-c('date', 'steps')
hist(steps_per_day$steps)
mean(steps_per_day$steps, na.rm=TRUE)
median(steps_per_day$steps, na.rm=TRUE)

by_interval = group_by(data, interval)
steps_per_interval = summarize(by_interval, mean(steps, na.rm=TRUE))
names(steps_per_interval)<-c('interval', 'steps')
steps_per_interval[which.max(steps_per_interval$steps), ]
mean_interval <- mean(steps_per_interval$steps)

nas <- is.na(data$steps)
num_nas <- sum(nas)
data_no_nas <- data
data_no_nas[nas, 'steps'] <- mean_interval
by_interval_no_nas = group_by(data_no_nas, interval)
steps_per_interval_no_nas = summarize(by_interval_no_nas, mean(steps, na.rm=TRUE))
names(steps_per_interval_no_nas)<-c('interval', 'steps')

steps_no_nas <- steps_per_interval_no_nas$steps
steps <- steps_per_interval$steps
stepsCombined <- rbind(data.frame(steps=steps, nas='Present'), data.frame(steps=steps_no_nas, nas='Filled with mean of intervals'))
ggplot(stepsCombined, aes(steps, fill = nas)) + geom_density(alpha = 0.2)

wkd <- weekdays(as.Date(data_no_nas$date))
weekday = wkd!='Sábado' & wkd !='Domingo'
weekday<-factor(weekday)
levels(weekday)[levels(weekday)==TRUE]='weekday'
levels(weekday)[levels(weekday)==FALSE]='weekend'
data_no_nas$daytype = weekday

by_interval_no_nas_weekday = group_by(data_no_nas[data_no_nas$daytype=='weekday',], interval)
steps_per_interval_no_nas_weekday = summarize(by_interval_no_nas_weekday,mean(steps, na.rm=TRUE))
names(steps_per_interval_no_nas_weekday)<-c('interval', 'steps')

by_interval_no_nas_weekend = group_by(data_no_nas[data_no_nas$daytype=='weekend',], interval)
steps_per_interval_no_nas_weekend = summarize(by_interval_no_nas_weekend, mean(steps, na.rm=TRUE))
names(steps_per_interval_no_nas_weekend)<-c('interval', 'steps')



p1 <- qplot(x='interval', y='steps', data=steps_per_interval_no_nas_weekday)
p2 <- qplot(x='interval', y='steps', data=steps_per_interval_no_nas_weekend)

multiplot(p1, p2, cols=2)
