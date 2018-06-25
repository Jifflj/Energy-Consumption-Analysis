library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

# read file
power_days <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/power_days.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)


# ----------------------------------------------------Preprocessing------------------------------------------------####

# set DateTime as GMT (readable format)
power_days$DateTime <- as.POSIXct(power_days$DateTime, tz ="GMT")

# filter seasons by months
winter <- filter(power_days, Month %in% c(12,1,2))
spring <- filter(power_days, Month %in% c(3,4,5))
summer <- filter(power_days, Month %in% c(6,7,8))
automn <- filter(power_days, Month %in% c(9,10,11))

#power_days <- mutate(power_days, month = as.numeric(format(month(power_days$DateTime))))

# divide into hour-intervals
winter <- mutate(winter, hour_of_day = as.numeric(format(hour(winter$DateTime))))
spring <- mutate(spring, hour_of_day = as.numeric(format(hour(spring$DateTime))))
summer <- mutate(summer, hour_of_day = as.numeric(format(hour(summer$DateTime))))
automn <- mutate(automn, hour_of_day = as.numeric(format(hour(automn$DateTime))))

# aggregate hours by mean
winter_day <- winter %>% aggregate(by = list (winter$hour_of_day), FUN = mean)
spring_day <- spring %>% aggregate(by = list (spring$hour_of_day), FUN = mean)
summer_day <- summer %>% aggregate(by = list (summer$hour_of_day), FUN = mean)
automn_day <- automn %>% aggregate(by = list (automn$hour_of_day), FUN = mean)


# cleaning out unnecessary columns ####

winter_day$Group.1 <- NULL
winter_day$X <- NULL
winter_day$DateTime <- NULL
winter_day$Month <- NULL
winter_day$Year <- NULL
winter_day$minutes <- NULL
winter_day$days <- NULL

spring_day$Group.1 <- NULL
spring_day$X <- NULL
spring_day$DateTime <- NULL
spring_day$Month <- NULL
spring_day$Year <- NULL
spring_day$minutes <- NULL
spring_day$days <- NULL

summer_day$Group.1 <- NULL
summer_day$X <- NULL
summer_day$DateTime <- NULL
summer_day$Month <- NULL
summer_day$Year <- NULL
summer_day$minutes <- NULL
summer_day$days <- NULL

automn_day$Group.1 <- NULL
automn_day$X <- NULL
automn_day$DateTime <- NULL
automn_day$Month <- NULL
automn_day$Year <- NULL
automn_day$minutes <- NULL
automn_day$days <- NULL


#--------------------------------------------------------PLOTTING---------------------------------------------------####

#--------------------Winter----------------####

# average active power winterday
pl_winter_day_av <- ggplot() + geom_line(data = winter_day, aes(x = hour_of_day, y = household_minute_averaged_active_power)) + scale_x_continuous(breaks = 0:23) + ggtitle("Average power consumption on a winter day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_winter_day_av)

# kitchen power winterday
pl_winter_day_kit <- ggplot() + geom_line(data = winter_day, aes(x = hour_of_day, y = kitchen)) + scale_x_continuous(breaks = 0:23) + ggtitle("Kitchen power consumption on a winter day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_winter_day_kit)

# laundry power winterday
pl_winter_day_laundry <- ggplot() + geom_line(data = winter_day, aes(x = hour_of_day, y = laundry_room)) + scale_x_continuous(breaks = 0:23) + ggtitle("Laundry power consumption on a winter day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_winter_day_laundry)

# water and air power winterday
pl_winter_day_water <- ggplot() + geom_line(data = winter_day, aes(x = hour_of_day, y = water_air)) + scale_x_continuous(breaks = 0:23) + ggtitle("Water and Air power consumption on a winter day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_winter_day_water)


# arrange grid for all 4 plots
grid.arrange(pl_winter_day_av,pl_winter_day_kit,pl_winter_day_laundry,pl_winter_day_water, nrow = 2,ncol = 2,top = text_grob ("Power consumption winter days", size=18))


#------------------Spring------------------####

# average active power springday
pl_spring_day_av <- ggplot() + geom_line(data = spring_day, aes(x = hour_of_day, y = household_minute_averaged_active_power)) + scale_x_continuous(breaks = 0:23) + ggtitle("Average power consumption on a spring day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_spring_day_av)

# kitchen power springday
pl_spring_day_kit <- ggplot() + geom_line(data = spring_day, aes(x = hour_of_day, y = kitchen)) + scale_x_continuous(breaks = 0:23) + ggtitle("Kitchen power consumption on a spring day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_spring_day_kit)

# laundry power springday
pl_spring_day_laundry <- ggplot() + geom_line(data = spring_day, aes(x = hour_of_day, y = laundry_room)) + scale_x_continuous(breaks = 0:23) + ggtitle("Laundry power consumption on a spring day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_spring_day_laundry)

# water and air power springday
pl_spring_day_water <- ggplot() + geom_line(data = spring_day, aes(x = hour_of_day, y = water_air)) + scale_x_continuous(breaks = 0:23) + ggtitle("Water and Air power consumption on a spring day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_spring_day_water)


# arrange grid for all 4 plots
grid.arrange(pl_spring_day_av,pl_spring_day_kit,pl_spring_day_laundry,pl_spring_day_water, nrow = 2,ncol = 2,top = text_grob ("Power consumption spring days", size=18))


#------------------Summer-------------------####

# average active power summerday
pl_summer_day_av <- ggplot() + geom_line(data = summer_day, aes(x = hour_of_day, y = household_minute_averaged_active_power)) + scale_x_continuous(breaks = 0:23) + ggtitle("Average power consumption on a summer day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_summer_day_av)

# kitchen power summerday
pl_summer_day_kit <- ggplot() + geom_line(data = summer_day, aes(x = hour_of_day, y = kitchen)) + scale_x_continuous(breaks = 0:23) + ggtitle("Kitchen power consumption on a summer day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_summer_day_kit)

# laundry power summerday
pl_summer_day_laundry <- ggplot() + geom_line(data = summer_day, aes(x = hour_of_day, y = laundry_room)) + scale_x_continuous(breaks = 0:23) + ggtitle("Laundry power consumption on a summer day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_summer_day_laundry)

# water and air power summerday
pl_summer_day_water <- ggplot() + geom_line(data = summer_day, aes(x = hour_of_day, y = water_air)) + scale_x_continuous(breaks = 0:23) + ggtitle("Water and Air power consumption on a summer day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_summer_day_water)



# arrange grid for all 4 plots
grid.arrange(pl_summer_day_av,pl_summer_day_kit,pl_summer_day_laundry,pl_summer_day_water, nrow = 2,ncol = 2,top = text_grob ("Power consumption summer days", size=18))


#------------------Automn--------------------####

# average active power automnday
pl_automn_day_av <- ggplot() + geom_line(data = automn_day, aes(x = hour_of_day, y = household_minute_averaged_active_power)) + scale_x_continuous(breaks = 0:23) + ggtitle("Average power consumption on a automn day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_automn_day_av)

# kitchen power automnday
pl_automn_day_kit <- ggplot() + geom_line(data = automn_day, aes(x = hour_of_day, y = kitchen)) + scale_x_continuous(breaks = 0:23) + ggtitle("Kitchen power consumption on a automn day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_automn_day_kit)

# laundry power automnday
pl_automn_day_laundry <- ggplot() + geom_line(data = automn_day, aes(x = hour_of_day, y = laundry_room)) + scale_x_continuous(breaks = 0:23) + ggtitle("Laundry power consumption on a automn day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_automn_day_laundry)

# water and air power automnday
pl_automn_day_water <- ggplot() + geom_line(data = automn_day, aes(x = hour_of_day, y = water_air)) + scale_x_continuous(breaks = 0:23) + ggtitle("Water and Air power consumption on a automn day") + theme(plot.title = element_text(hjust = 0.5))+xlab("Hours of day")+ylab("Power in Kilowatt per hour")
print(pl_automn_day_water)



# arrange grid for all 4 plots
grid.arrange(pl_automn_day_av,pl_automn_day_kit,pl_automn_day_laundry,pl_automn_day_water, nrow = 2,ncol = 2,top = text_grob ("Power consumption automn days", size=18))
