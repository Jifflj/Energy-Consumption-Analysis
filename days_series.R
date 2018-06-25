days_series <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/power_days.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

series_days_global <- ts(days_series$household_minute_averaged_active_power, frequency=10, start=c(2007,1))
series_days_kitchen <- ts(days_series$kitchen, frequency=10, start=c(2007,1))
series_days_laundry <- ts(days_series$laundry_room, frequency=10, start=c(2007,1))
series_days_wa <- ts(days_series$water_air, frequency=10, start=c(2007,1))

seasons_day_series <- ts(seasons_day$laundry_room, frequency=10)
plot(seasons_day_series)


series_winter_day_global <- ts(winter_day$household_minute_averaged_active_power, frequency=10, start=c(2007,1))
series_spring_day_global <- ts(spring_day$household_minute_averaged_active_power, frequency=10, start=c(2007))
series_summer_day_global <- ts(summer_day$household_minute_averaged_active_power, frequency=10, start=c(2007))
series_automn_day_global <- ts(automn_day$household_minute_averaged_active_power, frequency=10, start=c(2007))


plot(series_days_global)
plot(series_days_kitchen)
plot(series_days_laundry)
plot(series_days_wa)


plot(series_winter_day_global)
plot(series_spring_day_global)
plot(series_summer_day_global)
plot(series_automn_day_global)