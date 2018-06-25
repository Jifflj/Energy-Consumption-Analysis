library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# read file
power2008 <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task1/data/power2008.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

# call the head of file
head(power2008)

# -----------------------------------Preprocessing ----------------------------------------####

# create new column for year
power2008$Year <- 2008

# convert DateTime to GMT (readable format)
power2008$DateTime <- as.POSIXct(power2008$DateTime, tz ="GMT")

# drop NAs
power2008 %>% drop_na()

# keep only rows with content
power2008 <- power2008[complete.cases(power2008),]

# group by minutes
power2008$minutes <- minute(power2008$DateTime)

# filter in 15min intervals
power2008 <- filter(power2008, minutes %in% c(0,15,30,45))

# factorize Year
power2008$Year <- factor(power2008$Year)

# call str on file
str(power2008)

####---------------------------------Grouping/Aggregating (different options!)----------------------------------####

# group by months
month2008 <- group_by(power2008, Month = month(DateTime))

# group by season
season2008 <- group_by(power2008, Season = (quarter(DateTime)))

# group by weeks
week2008 <- group_by(power2008, Week = week(DateTime))

# group by days
day2008 <- aggregate(power2008, by = list (date(power2008$DateTime)), FUN = mean)

# divide by weekdays
power2008$days <- weekdays(as.Date(power2008$DateTime))

# divide into weekdays and week-emd-days
power2008_we <- filter(power2008, power2008$days == "Samstag"| power2008$days=="Sonntag")
power2008_wd <- filter(power2008, power2008$days != "Samstag" & power2008$days!="Sonntag")

# aggregate all months by mean
power2008_agg_month <- summarise_all(month2008, funs(mean))

# delete column
power2008_agg_month$DateTime <- NULL

# set Year as a factor
power2008_agg_month$Year <- factor(power2008_agg_month$Year)

# aggregate all seasons by mean
power2008_agg_seasons <- summarise_all(season2008, funs(mean))

# aggregate all weeks by mean
power2008_agg_week <- summarise_all(week2008, funs(mean))

power2008_agg_week$DateTime <- NULL

# aggregate all days by mean
power2008_agg_day <- summarise_all(day2008, funs(mean))