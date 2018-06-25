library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

power_we <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/power_we.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

power_we$DateTime <- as.POSIXct(power_we$DateTime, tz ="GMT")


# factorize Year
power_we$Year <- factor(power_we$Year)

####---------------------------------Grouping/Aggregating----------------------------------####

# group by season
seasons <- group_by(power_we, Season = (quarter(DateTime)))

power_we <- mutate(power_we, timetag = paste(Year,quarter(DateTime),days,formatC(hour(DateTime), width = 2, flag = "0"),sep = "/"))

power_we_timegrouped <- group_by(power_we, timetag)
power_we_agg <- summarise_all(power_we_timegrouped, funs(mean), na.rm =T)

power_we_sep <- separate(power_we_agg, timetag, c("Year", "Season", "Day","Hour"))

####---------------------------------------Plotting------------------------------------------####

# plot average power per season
pl_average_seasons <- ggplot() + geom_line(data = seasons, aes(x = Season, y = household_minute_averaged_active_power , color = year)) + scale_x_continuous(breaks = 1:4) + ggtitle("Average active power") + theme(plot.title = element_text(hjust = 0.5))+xlab("Month")+ylab("Power in Kilowatt per hour")


print(pl_seasons)

# plot power in kitchen per season
pl_kitchen_seasons <- ggplot() + geom_line(data = seasons, aes(x = Season, y = kitchen , color = Year)) + scale_x_continuous(breaks = 1:4) + ggtitle("Average power consumption in kitchen per season") + theme(plot.title = element_text(hjust = 0.5))+xlab("Season")+ylab("Power in Kilowatt per hour")


print(pl_kitchen_seasons)

# plot power in laundry room per season
pl_laundry_seasons <- ggplot() + geom_line(data = Seasons, aes(x = Season, y = laundry_room , color = Year)) + scale_x_continuous(breaks = 1:4) + ggtitle("Average power consumption in laundry room per season") + theme(plot.title = element_text(hjust = 0.5))+xlab("Season")+ylab("Power in Kilowatt per hour")

print(pl_laundry_seasons)

# plot power water heater and air conditioner per seaon
pl_water_air_seasons <- ggplot() + geom_line(data = Seasons, aes(x = Season, y = water_air , color = Year)) + scale_x_continuous(breaks = 1:4) + ggtitle("Average power consumption per season") + theme(plot.title = element_text(hjust = 0.5))+xlab("Season")+ylab("Power in Kilowatt per hour")

print(pl_water_air_seasons)
