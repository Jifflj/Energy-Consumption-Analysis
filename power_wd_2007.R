library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggpubr)

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

#### 2007 - season 1 ####
season1_2007 <- power_wd_sep %>% filter(Season == 1)
s1y2007 <- season1_2007 %>% filter(Year == 2007)
s1y2007$Day <- factor(s1y2007$Day,levels(s1y2007$Day)[c(5,1,4,2,3)])
d1y2007s1 <- s1y2007 %>% filter(Day == "Montag")
d2y2007s1 <- s1y2007 %>% filter(Day == "Dienstag")
d3y2007s1 <- s1y2007 %>% filter(Day == "Mittwoch")
d4y2007s1 <- s1y2007 %>% filter(Day == "Donnerstag")
d5y2007s1 <- s1y2007 %>% filter(Day == "Freitag")


#### 2007 - season 2 ####
season2_2007 <- power_wd_sep %>% filter(Season == 2)
s2y2007 <- season2_2007 %>% filter(Year == 2007)
d1y2007s2 <- s2y2007 %>% filter(Day == "Montag")
d2y2007s2 <- s2y2007 %>% filter(Day == "Dienstag")
d3y2007s2 <- s2y2007 %>% filter(Day == "Mittwoch")
d4y2007s2 <- s2y2007 %>% filter(Day == "Donnerstag")
d5y2007s2 <- s2y2007 %>% filter(Day == "Freitag")


#### 2007 - season 3 ####
season3_2007 <- power_wd_sep %>% filter(Season == 3)
s3y2007 <- season3_2007 %>% filter(Year == 2007)
d1y2007s3 <- s3y2007 %>% filter(Day == "Montag")
d2y2007s3 <- s3y2007 %>% filter(Day == "Dienstag")
d3y2007s3 <- s3y2007 %>% filter(Day == "Mittwoch")
d4y2007s3 <- s3y2007 %>% filter(Day == "Donnerstag")
d5y2007s3 <- s3y2007 %>% filter(Day == "Freitag")


#### 2007 - season 4 ####
season4_2007 <- power_wd_sep %>% filter(Season == 4)
s4y2007 <- season4_2007 %>% filter(Year == 2007)
d1y2007s4 <- s4y2007 %>% filter(Day == "Montag")
d2y2007s4 <- s4y2007 %>% filter(Day == "Dienstag")
d3y2007s4 <- s4y2007 %>% filter(Day == "Mittwoch")
d4y2007s4 <- s4y2007 %>% filter(Day == "Donnerstag")
d5y2007s4 <- s4y2007 %>% filter(Day == "Freitag")


####---------------------------------------Plotting------------------------------------------####

####-------------------Season 1-------------------####

# plot average power
pl_week_2007s1 <- ggplot() + 
  
  geom_line(data = d1y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d5y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  #scale_color_manual(values = c())
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Average Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

print(pl_week_2007s1)


# plot kitchen power
pl_week_2007s1_kit <- ggplot() + 
  
  geom_line(data = d1y2007s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2007s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2007s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2007s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2007s1, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day), size=1.5) +
  
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Kitchen Power weekdays season 1,2007") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

print(pl_week_2007s1_kit)

# plot laundry power
pl_week_2007s1_laund <- ggplot() + 
  
  geom_line(data = d1y2007s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2007s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2007s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2007s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2007s1, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day), size=1.5) +
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Laundry Power consumption weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  
  ylab("Power in Kilowatt")

print(pl_week_2007s1_laund)

# plot water and air power
pl_week_2007s1_wa <- ggplot() + 
  
  geom_line(data = d1y2007s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2007s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2007s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2007s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2007s1, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day), size=1.5) +
  theme(panel.background = element_rect(fill = "gray70"), plot.background = element_rect(fill = "gray70")) +
  
  scale_x_continuous(name= "Hours of the day",breaks = c(0,4,8,12,16,20,23),labels = c("0 a.m","4 a.m", "8 a.m", "12 a.m", "16 a.m", "20 a.m", "23 a.m" )) + 
  
  ggtitle("Water and Air Power consumption on weekdays 2007, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  ylab("Power in Kilowatt")

print(pl_week_2007s1_wa)


grid.arrange(pl_week_2007s1,pl_week_2007s1_kit,pl_week_2007s1_laund,pl_week_2007s1_wa, nrow = 2,ncol = 2,top = text_grob ("Power consumption season 1, 2007", size=18))


####-------------------Season 2-------------------####

# plot average power
pl_week_2007s2 <- ggplot() + 
  
  geom_line(data = d1y2007s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2007s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2007s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day), size=1.5) +
  geom_line(data = d4y2007s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d4y2007s2, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day),size=1.5) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s2)


# plot kitchen power
pl_week_2007s2_kit <- ggplot() + 
  
  geom_line(data = d1y2007s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2007s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2007s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d4y2007s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2007s2, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s2_kit)

# plot laundry power
pl_week_2007s2_laund <- ggplot() + 
  
  geom_line(data = d1y2007s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2007s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2007s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d4y2007s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2007s2, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s2_laund)

# plot water and air power
pl_week_2007s2_wa <- ggplot() + 
  
  geom_line(data = d1y2007s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2007s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2007s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d4y2007s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2007s2, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2007, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s2_wa)



####-------------------Season 3-------------------####
# plot average power
pl_week_2007s3 <- ggplot() + 
  
  geom_line(data = d1y2007s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2007s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2007s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d4y2007s3, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s3)


# plot kitchen power
pl_week_2007s3_kit <- ggplot() + 
  
  geom_line(data = d1y2007s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2007s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2007s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d4y2007s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2007s3, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s3_kit)

# plot laundry power
pl_week_2007s3_laund <- ggplot() + 
  
  geom_line(data = d1y2007s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2007s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2007s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d4y2007s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2007s3, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s3_laund)

# plot water and air power
pl_week_2007s3_wa <- ggplot() + 
  
  geom_line(data = d1y2007s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2007s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2007s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d4y2007s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2007s3, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2007, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s3_wa)

####-------------------Season 4-------------------####

# plot average power
pl_week_2007s4 <- ggplot() + 
  
  geom_line(data = d1y2007s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d2y2007s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2007s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d4y2007s4, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s4)


# plot kitchen power
pl_week_2007s4_kit <- ggplot() + 
  
  geom_line(data = d1y2007s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) + 
  geom_line(data = d2y2007s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d3y2007s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d4y2007s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  geom_line(data = d5y2007s4, aes(x = as.numeric(Hour), y = kitchen, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Kitchen Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s4_kit)

# plot laundry power
pl_week_2007s4_laund <- ggplot() + 
  
  geom_line(data = d1y2007s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) + 
  geom_line(data = d2y2007s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d3y2007s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d4y2007s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  geom_line(data = d5y2007s4, aes(x = as.numeric(Hour), y = laundry_room, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Laundry Power consumption weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s4_laund)

# plot water and air power
pl_week_2007s4_wa <- ggplot() + 
  
  geom_line(data = d1y2007s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) + 
  geom_line(data = d2y2007s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d3y2007s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d4y2007s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  geom_line(data = d5y2007s4, aes(x = as.numeric(Hour), y = water_air, 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Water and Air Power consumption on weekdays 2007, season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s4_wa)
