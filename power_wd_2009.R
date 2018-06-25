library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

power_wd <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/power_wd.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

power_wd$DateTime <- as.POSIXct(power_wd$DateTime, tz ="GMT")

# factorize Year
power_wd$Year <- factor(power_wd$Year)

####----------------------------------Grouping/Aggregating----------------------------------####

# group by season
seasons <- group_by(power_wd, Season = (quarter(DateTime)))


power_wd <- mutate(power_wd, timetag = paste(Year,quarter(DateTime),days,formatC(hour(DateTime), width = 2, flag = "0"),sep = "/"))

power_wd_timegrouped <- group_by(power_wd, timetag)
power_wd_agg <- summarise_all(power_wd_timegrouped, funs(mean), na.rm = T)

power_wd_sep <- separate(power_wd_agg, timetag, c("Year", "Season", "Day","Hour"))

#### 2009 - season 1 ####
season1_2009 <- power_wd_sep %>% filter(Season == 1)
s1y2009 <- season1_2009 %>% filter(Year == 2009)
d1y2009s1 <- s1y2009 %>% filter(Day == "Montag")
d2y2009s1 <- s1y2009 %>% filter(Day == "Dienstag")
d3y2009s1 <- s1y2009 %>% filter(Day == "Mittwoch")
d4y2009s1 <- s1y2009 %>% filter(Day == "Donnerstag")
d5y2009s1 <- s1y2009 %>% filter(Day == "Freitag")


#### 2009 - season 2 ####
season2_2009 <- power_wd_sep %>% filter(Season == 2)
s2y2009 <- season2 %>% filter(Year == 2009)
d1y2009s2 <- s2y2009 %>% filter(Day == "Montag")
d2y2009s2 <- s2y2009 %>% filter(Day == "Dienstag")
d3y2009s2 <- s2y2009 %>% filter(Day == "Mittwoch")
d4y2009s2 <- s2y2009 %>% filter(Day == "Donnerstag")
d5y2009s2 <- s2y2009 %>% filter(Day == "Freitag")


#### 2009 - season 3 ####
season3_2009 <- power_wd_sep %>% filter(Season == 3)
s3y2009 <- season3 %>% filter(Year == 2009)
d1y2009s3 <- s3y2009 %>% filter(Day == "Montag")
d2y2009s3 <- s3y2009 %>% filter(Day == "Dienstag")
d3y2009s3 <- s3y2009 %>% filter(Day == "Mittwoch")
d4y2009s3 <- s3y2009 %>% filter(Day == "Donnerstag")
d5y2009s3 <- s3y2009 %>% filter(Day == "Freitag")


#### 2009 - season 4 ####
season4_2009 <- power_wd_sep %>% filter(Season == 4)
s4y2009 <- season4 %>% filter(Year == 2009)
d1y2009s4 <- s4y2009 %>% filter(Day == "Montag")
d2y2009s4 <- s4y2009 %>% filter(Day == "Dienstag")
d3y2009s4 <- s4y2009 %>% filter(Day == "Mittwoch")
d4y2009s4 <- s4y2009 %>% filter(Day == "Donnerstag")
d5y2009s4 <- s4y2009 %>% filter(Day == "Freitag")



####---------------------------------------Plotting------------------------------------------####

# plot average power season1 2009 monday - thursday
pl_week_2009s1 <- ggplot() + 
  
  geom_line(data = d1y2009s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2009s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2009s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d4y2009s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekdays 2009 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2009s1)


# plot kitchen power season1 2009 monday - thursday
pl_week_2009s1_kit <- ggplot() + 
  
  geom_line(data = d1y2009s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2009s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2009s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d4y2009s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2009s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekdays 2009 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2009s1_kit)

# plot laundry power season1 2009 monday - thursday
pl_week_2009s1_laund <- ggplot() + 
  
  geom_line(data = d1y2009s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2009s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2009s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d4y2009s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2009s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2009 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2009s1_laund)

# plot water and air power season1 2009 monday - thursday
pl_week_2009s1_wa <- ggplot() + 
  
  geom_line(data = d1y2009s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2009s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2009s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d4y2009s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2009s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2009, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2009s1_wa)


