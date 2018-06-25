library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# read file
power2007 <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task1/data/power2007.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

# ------------------------------------- Preprocessing ------------------------------------ ####

# create new column for year
power2007$Year <- 2007

# convert DateTime to GMT (readable format)
power2007$DateTime <- as.POSIXct(power2007$DateTime, tz ="GMT")

# drop NAs
power2007 %>% drop_na()

# keep only rows with content
power2007 <- power2007[complete.cases(power2007),]

# group by minutes
power2007$minutes <- minute(power2007$DateTime)

# filter in 15min intervals
power2007 <- filter(power2007, minutes %in% c(0,15,30,45))

# call the head of file 
head(power2007)

# ---------------------------------Grouping/Aggregating (different options!)----------------------------------####

# group by month
month2007 <- group_by(power2007, Month = month(DateTime))

# group by season
season2007 <- group_by(power2007, Season = (quarter(DateTime)))

# group by weeks
week2007 <- group_by(power2007, Week = week(DateTime))

#group by days
day2007 <- aggregate(power2007, by = list (date(power2007$DateTime)), FUN = mean)

# divide by weekdays
power2007$days <- weekdays(as.Date(power2007$DateTime))

# divide into weekdays and week-end-days
power2007_we <- filter(power2007, power2007$days == "Samstag" | power2007$days == "Sonntag")
power2007_wd <- filter(power2007, power2007$days != "Samstag" & power2007$days != "Sonntag")

# aggregate all months by mean
power2007_agg_month <- summarise_all(month2007, funs(mean))

# delete column
power2007_agg_month$DateTime <- NULL

# set Year as a factor
power2007_agg_month$Year <- factor(power2007_agg_month$Year)

# aggregate all seasons by mean
power2007_agg_season <- summarise_all(season2007, funs(mean))

# aggregate all weeks by mean
power2007_agg_week <- summarise_all(week2007, funs(mean))

power2007_agg_week$DateTime <- NULL

# aggregate all days by mean
power2007_agg_day <- summarise_all(day2007, funs(mean))