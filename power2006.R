library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(chron)

power2006 <- read.csv("~/Documents/Ubiqum/Course4/data/power2006.csv", sep=",",na.strings = c("?","NA"), fill = T, stringsAsFactors = FALSE)

# create new column for year
power2006$Year <- 2006

power2006$DateTime <- as.POSIXct(power2006$DateTime, tz ="GMT")

power2006 %>% drop_na()

# keep only rows with content
power2006 <- power2006[complete.cases(power2006),]

# factorize Year
power2006$Year <- factor(power2006$Year)

str(power2006)
head(power2006)
####---------------------------------Preprocessing----------------------------------####

# group by month
my_group_month2006 <- group_by(power2006, Month = month(DateTime))

# group by season
my_group_season2006 <- group_by(power2006, Season = (quarter(DateTime)))

# group by weeks
#my_group_week2006 <- group_by(power2006, Week = week(DateTime))

my_group_week2006 <- aggregate(power2006, by = list (week(power2006$DateTime)), FUN = mean)

#group by days
#my_group_day2006 <- group_by(power2006, Day = day(DateTime))
my_group_day2006 <- aggregate(power2006, by = list (date(power2006$DateTime)), FUN = mean)

my_group_day2006$days <- weekdays(as.Date(my_group_day2006$DateTime))

my_group_day2006_we <- filter(my_group_day2006, my_group_day2006$days == "Samstag"| my_group_day2006$days=="Sonntag")
my_group_day2006_wd <- filter(my_group_day2006, my_group_day2006$days != "Samstag" & my_group_day2006$days!="Sonntag")

# group by minutes
power2006$minutes <- minute(power2006$DateTime)
my_group_min2006_int <- filter(power2006, minutes %in% c(0,15,30,45))
#my_group_min2006_int <- aggregate(power2006, by = list (date(power2006$DateTime)), FUN = mean)

# aggregate all months by mean
power2006_agg <- summarise_all(my_group_month2006, funs(mean))

power2006_agg_month$DateTime <- NULL
power2006_agg_month$Year <- factor(power2006_agg_month$Year)

# divide all weeks in weekdays and weekend-days
power2006_agg_week <- summarise_all(my_group_week2006, funs(mean))

power2006_agg_week$DateTime <- NULL
power2006_agg_week$Month <- factor(power2006_agg_week$Month)

# aggregate all days by mean
power2006_agg_day <- summarise_all(my_group_day2006, funs(mean))



####---------------------------------------Plotting------------------------------------------####

# plot average power per month
pl_power2006_av_month <- ggplot(power2006_agg, aes(x=Month, y=household_minute_averaged_active_power))+
  geom_point()

print(pl_power2006_av_month)

# plot power in kitchen per month
pl_power2006_kitchen <- ggplot(power2006_agg, aes(x=Month, y=kitchen))+
  geom_point()

print(pl_power2006_kitchen)

# plot power in laundry room per month
pl_power2006_laundry <- ggplot(power2006_agg, aes(x=Month, y=laundry_room))+
  geom_point()

print(pl_power2006_laundry)

# plot power water heater and air conditioner per month

pl_power2006_water_air <- ggplot(power2006_agg, aes(x=Month, y=water_air))+
  geom_point()

print(pl_power2006_water_air)

####---------------------------------Modifying-------------------------------------####

