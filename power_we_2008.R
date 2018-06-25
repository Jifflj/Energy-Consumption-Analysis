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

#### 2008 - season 1 ####
season1_2008 <- power_wd_sep %>% filter(Season == 1)
s2y2008 <- season1_2008 %>% filter(Year == 2008)

d6y2008s2 <- s2y2008 %>% filter(Day == "Samstag")
d7y2008s2 <- s2y2008 %>% filter(Day == "Sonntag")

#### 2008 - season 2 ####

season2_2008 <- power_wd_sep %>% filter(Season == 2)
s2y2008 <- season2_2008 %>% filter(Year == 2008)

d6y2008s2 <- s2y2008 %>% filter(Day == "Samstag")
d7y2008s2 <- s2y2008 %>% filter(Day == "Sonntag")

#### 2008 - season 3 ####
season3_2008 <- power_wd_sep %>% filter(Season == 3)
s3y2008 <- season3_2008 %>% filter(Year == 2008)

d6y2008s3 <- s3y2008 %>% filter(Day == "Samstag")
d7y2008s3 <- s3y2008 %>% filter(Day == "Sonntag")

#### 2008 - season 4 ####
season4_2008 <- power_wd_sep %>% filter(Season == 4)
s4y2008 <- season4_2008 %>% filter(Year == 2008)

d6y2008s34 <- s4y2008 %>% filter(Day == "Samstag")
d7y2008s4 <- s4y2008 %>% filter(Day == "Sonntag")

####---------------------------------------Plotting------------------------------------------####

####------------------Season 1------------------####

# plot average power
pl_weekend_2008s1_av <- ggplot() + 
  
  geom_line(data = d6y2008s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2008s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s1_av)

# plot kitchen power
pl_weekend_2008s1_kit <- ggplot() + 
  
  geom_line(data = d6y2008s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2008s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s1_kit)

# plot laundry power
pl_weekend_2008s1_laund <- ggplot() + 
  
  geom_line(data = d6y2008s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2008s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s1_laund)

# plot water and air power
pl_weekend_2008s1_wa <- ggplot() + 
  
  geom_line(data = d1y2008s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2008s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s1_wa)

####------------------------Season 2----------------------####

# plot average power
pl_weekend_2008s2_av <- ggplot() + 
  
  geom_line(data = d6y2008s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2008s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s2_av)

# plot kitchen power
pl_weekend_2008s2_kit <- ggplot() + 
  
  geom_line(data = d6y2008s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2008s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s2_kit)

# plot laundry power
pl_weekend_2008s2_laund <- ggplot() + 
  
  geom_line(data = d6y2008s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2008s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s2_laund)

# plot water and air power
pl_weekend_2008s2_wa <- ggplot() + 
  
  geom_line(data = d1y2008s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2008s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s2_wa)

####-----------------------Season 3---------------------------####

# plot average power
pl_weekend_2008s3_av <- ggplot() + 
  
  geom_line(data = d6y2008s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2008s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s3_av)

# plot kitchen power
pl_weekend_2008s3_kit <- ggplot() + 
  
  geom_line(data = d6y2008s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2008s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s3_kit)

# plot laundry power
pl_weekend_2008s3_laund <- ggplot() + 
  
  geom_line(data = d6y2008s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2008s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s3_laund)

# plot water and air power
pl_weekend_2008s3_wa <- ggplot() + 
  
  geom_line(data = d1y2008s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2008s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s3_wa)

####--------------------------Season 4-----------------------------####
# plot average power
pl_weekend_2008s4_av <- ggplot() + 
  
  geom_line(data = d6y2008s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2008s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekends 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s4_av)

# plot kitchen power
pl_weekend_2008s4_kit <- ggplot() + 
  
  geom_line(data = d6y2008s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d7y2008s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekends 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s4_kit)

# plot laundry power
pl_weekend_2008s4_laund <- ggplot() + 
  
  geom_line(data = d6y2008s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d7y2008s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2008 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s4_laund)

# plot water and air power
pl_weekend_2008s4_wa <- ggplot() + 
  
  geom_line(data = d1y2008s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2008s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2008, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2008s4_wa)
