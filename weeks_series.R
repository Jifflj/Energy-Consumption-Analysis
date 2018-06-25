weeks_series <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/weeks_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

#### weeks' series ####
weeks_global <- ts(weeks_series$household_minute_averaged_active_power, frequency=53, start=c(2007,1))
weeks_kitchen <- ts(weeks_series$kitchen, frequency=53, start=c(2007,1))
weeks_laundry <- ts(weeks_series$laundry_room, frequency=53, start=c(2007,1))
weeks_wa <- ts(weeks_series$water_air, frequency=53, start=c(2007,1))

# plots
plot(weeks_global)
plot(weeks_kitchen)
plot(weeks_laundry)
plot(weeks_wa)

#### decomposed weekly over years ####
week_series_global_dc <- decompose(weeks_global)
week_series_kitchen_dc <- decompose(weeks_kitchen)
week_series_laundry_dc <- decompose(weeks_laundry)
week_series_wa_dc <- decompose(weeks_wa)

# plot decomposed wekly
plot(week_series_global_dc)
plot(week_series_kitchen_dc)
plot(week_series_laundry_dc)
plot(week_series_wa_dc)
