library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

# read file
power2010 <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task1/data/power2010.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)


# -------------------------------------Preprocessing---------------------------------------####

# create new column for year
power2010$Year <- 2010

# convert DateTime to GMT (readable format)
power2010$DateTime <- as.POSIXct(power2010$DateTime, tz ="GMT")

# drop NAs
power2010 %>% drop_na()

# keep only rows with content
power2010 <- power2010[complete.cases(power2010),]

# group by minutes
power2010$minutes <- minute(power2010$DateTime)

# filter in 15min intervals
min2010_int <- filter(power2010, minutes %in% c(0,15,30,45))

# call the head of the file
head(power2010)

####---------------------------------Grouping/Aggregating (different options!)----------------------------------####

# group by month
month2010 <- group_by(power2010, Month = month(DateTime))

# group by season
season2010 <- group_by(power2010, Season = (quarter(DateTime)))

# group by weeks
week2010 <- group_by(power2010, Week = week(DateTime))

#group by days
day2010 <- aggregate(power2010, by = list (date(power2010$DateTime)), FUN = mean)

# divide by weekdays
power2010$days <- weekdays(as.Date(power2010$DateTime))

# divide into weekdays and week-emd-days
power2010_we <- filter(power2010, power2010$days == "Samstag"| power2010$days=="Sonntag")
power2010_wd <- filter(power2010, power2010$days != "Samstag" & power2010$days!="Sonntag")

# aggregate all months by mean
power2010_agg_month <- summarise_all(month2010, funs(mean))

# delete column
power2010_agg_month$DateTime <- NULL

# set Year as a factor
power2010_agg_month$Year <- factor(power2010_agg_month$Year)

# aggregate all seasons by mean
seasons2010_agg <- summarise_all(season2010, funs(mean))

# aggregate all weeks by mean
power2010_agg_week <- summarise_all(week2010, funs(mean))

# delete column
power2010_agg_week$DateTime <- NULL

# aggregate all days by mean
power2010_agg_day <- summarise_all(day2010, funs(mean))
