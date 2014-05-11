diff_dates=unique(data$date)
steps_by_day=rep.int(0,length(diff_dates))
iter=1:length(diff_dates)
for(i in iter)
    steps_by_day[i]=sum(data[data$date==diff_dates[i],"steps"],na.rm=TRUE)
barplot(steps_by_day,names.arg=diff_dates)

intervals=unique(data$interval)
interval_steps_over_days=rep(0,length(intervals))
interval_iter=1:length(intervals)
for(i in interval_iter)
    interval_steps_over_days[i]=mean(data[data$interval==intervals[i],"steps"],na.rm=TRUE)
plot(intervals, interval_steps_over_days, type="l", col="darkblue")

