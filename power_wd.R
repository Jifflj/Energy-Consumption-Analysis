library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

power_wd <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/days/power_days.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

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

#### 2007 ####
# season 1 #
season1_2007 <- power_wd_sep %>% filter(Season == 1)
s1y2007 <- season1_2007 %>% filter(Year == 2007)
d1y2007s1 <- s1y2007 %>% filter(Day == "Montag")
d2y2007s1 <- s1y2007 %>% filter(Day == "Dienstag")
d3y2007s1 <- s1y2007 %>% filter(Day == "Mittwoch")
d4y2007s1 <- s1y2007 %>% filter(Day == "Donnerstag")
d5y2007s1 <- s1y2007 %>% filter(Day == "Freitag")
d6y2007s1 <- s1y2007 %>% filter(Day == "Samstag")
d7y2007s1 <- s1y2007 %>% filter(Day == "Sonntag")

# season 2 #
season2_2007 <- power_wd_sep %>% filter(Season == 2)
s2y2007 <- season2 %>% filter(Year == 2007)
d1y2007s2 <- s2y2007 %>% filter(Day == "Montag")
d2y2007s2 <- s2y2007 %>% filter(Day == "Dienstag")
d3y2007s2 <- s2y2007 %>% filter(Day == "Mittwoch")
d4y2007s2 <- s2y2007 %>% filter(Day == "Donnerstag")
d5y2007s2 <- s2y2007 %>% filter(Day == "Freitag")
d6y2007s2 <- s2y2007 %>% filter(Day == "Samstag")
d7y2007s2 <- s2y2007 %>% filter(Day == "Sonntag")

# season 3 #
season3_2007 <- power_wd_sep %>% filter(Season == 3)
s3y2007 <- season3 %>% filter(Year == 2007)
d1y2007s3 <- s3y2007 %>% filter(Day == "Montag")
d2y2007s3 <- s3y2007 %>% filter(Day == "Dienstag")
d3y2007s3 <- s3y2007 %>% filter(Day == "Mittwoch")
d4y2007s3 <- s3y2007 %>% filter(Day == "Donnerstag")
d5y2007s3 <- s3y2007 %>% filter(Day == "Freitag")
d6y2007s3 <- s3y2007 %>% filter(Day == "Samstag")
d7y2007s3 <- s3y2007 %>% filter(Day == "Sonntag")

# season 4 #
season4_2007 <- power_wd_sep %>% filter(Season == 4)
s4y2007 <- season4 %>% filter(Year == 2007)
d1y2007s4 <- s4y2007 %>% filter(Day == "Montag")
d2y2007s4 <- s4y2007 %>% filter(Day == "Dienstag")
d3y2007s4 <- s4y2007 %>% filter(Day == "Mittwoch")
d4y2007s4 <- s4y2007 %>% filter(Day == "Donnerstag")
d5y2007s4 <- s4y2007 %>% filter(Day == "Freitag")
d6y2007s4 <- s4y2007 %>% filter(Day == "Samstag")
d7y2007s4 <- s4y2007 %>% filter(Day == "Sonntag")


####---------------------------------------Plotting------------------------------------------####

# plot average power season1 2007 monday - thursday
pl_week_2007s1 <- ggplot() + 
  
  geom_line(data = d1y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                                        color = Day)) + 
  geom_line(data = d2y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d3y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  geom_line(data = d4y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power weekdays 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_week_2007s1)

# plot average power season1 2007 friday - sunday
pl_weekend_2007s1 <- ggplot() + 
  geom_line(data = d5y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d6y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) + 
  geom_line(data = d7y2007s1, aes(x = as.numeric(Hour), y = household_minute_averaged_active_power , 
                                  color = Day)) +
  scale_x_discrete(breaks = 0:23) + 
  ggtitle("Average Power Friday - Sunday 2007 season 1") + theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hours of the day")+ylab("Power in Kilowatt")

print(pl_weekend_2007s1)
