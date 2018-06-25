library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# read file
power2009 <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task1/data/power2009.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

# -------------------------------------Preprocessing---------------------------------------####

# create new column for year
power2009$Year <- 2009

# convert DateTime to GMT (readable format)
power2009$DateTime <- as.POSIXct(power2009$DateTime, tz ="GMT")

# drop NAs
power2009 %>% drop_na()

# group by minutes
power2009$minutes <- minute(power2009$DateTime)

# filter in 15min intervals
power2009 <- filter(power2009, minutes %in% c(0,15,30,45))

# keep only rows with content
power2009 <- power2009[complete.cases(power2009),]

# call the head of the file
head(power2009)

####---------------------------------Grouping/Aggregating (different options!)----------------------------------####

# group by month
month2009 <- group_by(power2009, Month = month(DateTime))

# group by season
season2009 <- group_by(power2009, Season = (quarter(DateTime)))

# group by weeks
week2009 <- group_by(power2009, Week = week(DateTime))

#group by days
day2009 <- aggregate(power2009, by = list (date(power2009$DateTime)), FUN = mean)

# divide by weekdays
power2009$days <- weekdays(as.Date(power2009$DateTime))

# divide into weekdays and week-emd-days
power2009_we <- filter(power2009, power2009$days == "Samstag"| power2009$days=="Sonntag")
power2009_wd <- filter(power2009, power2009$days != "Samstag" & power2009$days!="Sonntag")

# aggregate all months by mean
power2009_agg <- summarise_all(month2009, funs(mean))

# delte column
power2009_agg_month$DateTime <- NULL

# set Year as a factor
power2009_agg_month$Year <- factor(power2009_agg_month$Year)

# aggregate all seasons by mean
seasons2009_agg <- summarise_all(season2009, funs(mean)) 

# aggregate all weeks by mean
power2009_agg_week <- summarise_all(week2009, funs(mean))

# delete column
power2009_agg_week$DateTime <- NULL

# aggregate all days by mean
power2009_agg_day <- summarise_all(day2009, funs(mean))