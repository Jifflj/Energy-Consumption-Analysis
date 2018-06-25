library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# read file
hhpower <- read.csv("~/Documents/Ubiqum/Course4/data/household_power_consumption.txt", sep=";",na.strings = c("?"), fill = T)

# convert Kilowatt to Watthour + create new column for measurement loss by submetering
hhpower <- mutate(hhpower, Global_active_power = Global_active_power * 100 / 6 )
hhpower <- mutate(hhpower, Global_reactive_power = Global_reactive_power * 100 / 6 )
hhpower <- mutate(hhpower, not_measured_in_sub = Global_active_power - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

# summarize hhpower
summary(hhpower)

# rename columns
hhpower <- rename(hhpower, household_minute_averaged_active_power = Global_active_power)
hhpower <- rename(hhpower, household_minute_averaged_reactive_power = Global_reactive_power)
hhpower <- rename(hhpower, household_minute_averaged_current_intensity_ampere = Global_intensity) 
hhpower <- rename(hhpower, minute_averaged_voltage = Voltage)
hhpower <- rename(hhpower, kitchen = Sub_metering_1)
hhpower <- rename(hhpower, laundry_room = Sub_metering_2)
hhpower <- rename(hhpower, water_air = Sub_metering_3)


#Dplyr version of cbind for Date + Time
hhpower <- mutate(hhpower, DateTime = paste(Date,Time))

# cbind Date + Time
#hhpower <-cbind(hhpower,paste(hhpower$Date,hhpower$Time), stringsAsFactors=FALSE)
#colnames(hhpower)[10] <-"DateTime"
hhpower <- hhpower[,c(ncol(hhpower), 1:(ncol(hhpower)-1))]


# delete unneeded columns
hhpower$Date <- NULL
hhpower$Time <- NULL
hhpower$household_minute_averaged_reactive_power <- NULL
hhpower$minute_averaged_voltage <- NULL
hhpower$household_minute_averaged_current_intensity_ampere <- NULL

# convert DateTime to readable format
hhpower$DateTime <- strptime(hhpower$DateTime, "%d/%m/%Y %H:%M:%S")
hhpower$DateTime <- as.POSIXct(hhpower$DateTime, tz ="GMT")
hhpower$Month <- month(hhpower$DateTime)

# function for converting Kilowatt to Watthour
#Kw2wh <- function(kw){
  #wh = kw*100/6
  #return(wh)
#}

# apply function
#hhpower$household_minute_averaged_active_power <- Kw2wh(hhpower$household_minute_averaged_active_power)
#hhpower$household_minute_averaged_reactive_power <- Kw2wh(hhpower$household_minute_averaged_reactive_power)

summary(hhpower)

# filter by year(s)
power2006 <- filter(hhpower, year(DateTime)=="2006")
power2007 <- filter(hhpower, year(DateTime)=="2007")
power2008 <- filter(hhpower, year(DateTime)=="2008")
power2009 <- filter(hhpower, year(DateTime)=="2009")
power2010 <- filter(hhpower, year(DateTime)=="2010")

# write csv-files for each year
write.csv(power2006, file = "power2006.csv", row.names = FALSE)
write.csv(power2007, file = "power2007.csv", row.names = FALSE)
write.csv(power2008, file = "power2008.csv", row.names = FALSE)
write.csv(power2009, file = "power2009.csv", row.names = FALSE)
write.csv(power2010, file = "power2010.csv", row.names = FALSE)
