library(forecast)
library(ggplot2)
library(reshape)

season1_years <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_years/season1_years.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
season2_years <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_years/season2_years.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
season3_years <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_years/season3_years.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)
season4_years <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_years/season4_years.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

#### HW function ####
HWplot <- function(series_season1_global,  n.ahead = 12,  CI = .95,  error.ribbon = 'green', line.size = 1){
  
  hw <- HoltWinters(series_season1_global)
  
  forecast <- predict(hw, n.ahead = n.ahead,  prediction.interval = T,  level = CI)
  
  
  for_values <- data.frame(time = round(time(forecast),  3),  value_forecast = as.data.frame(forecast)$fit,  dev = as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values <- data.frame(time = round(time(hw$fitted),  3),  value_fitted = as.data.frame(hw$fitted)$xhat)
  
  actual_values <- data.frame(time = round(time(hw$x),  3),  Actual = c(hw$x))
  
  
  graphset <- merge(actual_values,  fitted_values,  by = 'time',  all = TRUE)
  graphset <- merge(graphset,  for_values,  all = TRUE,  by = 'time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted <- c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt <- melt(graphset[, c('time', 'Actual', 'Fitted')], id = 'time')
  
  p <- ggplot(graphset.melt,  aes(x = time,  y = value)) + 
    geom_ribbon(data = graphset, aes(x = time, y = Fitted, ymin = Fitted-dev,  ymax = Fitted + dev),  alpha = .2,  fill = error.ribbon) + 
    geom_line(aes(colour = variable), size = line.size) + geom_vline(x = max(actual_values$time),  xintercept = 2) + 
    xlab('Time') + ylab('Global Consumption Winter') + ggtitle("Holt-Winters forecast") + theme(plot.title = element_text(hjust = 0.5))
  return(p)
  
}



#### TS SEASONS ####
# Season 1
series_season1_global <- ts(season1_years$household_minute_averaged_active_power, frequency=3, start=c(2007,1))
series_season1_kitchen <- ts(season1_years$kitchen, frequency=3, start=c(2007,1))
series_season1_laundry <- ts(season1_years$laundry_room, frequency=3, start=c(2007,1))
series_season1_wa <- ts(season1_years$water_air, frequency=3, start=c(2007,1))

# plot season 1 series
plot.ts(series_season1_global)
plot.ts(series_season1_kitchen)
plot.ts(series_season1_laundry)
plot.ts(series_season1_wa)

# Season 2
series_season2_global <- ts(season2_years$household_minute_averaged_active_power, frequency=3, start=c(2007,1))
series_season2_kitchen <- ts(season2_years$kitchen, frequency=3, start=c(2007,1))
series_season2_laundry <- ts(season2_years$laundry_room, frequency=3, start=c(2007,1))
series_season2_wa <- ts(season2_years$water_air, frequency=3, start=c(2007,1))

# plot season 2 series
plot.ts(series_season1_global)
plot.ts(series_season2_kitchen)
plot.ts(series_season2_laundry)
plot.ts(series_season2_wa)

# Season 3
series_season3_global <- ts(season3_years$household_minute_averaged_active_power, frequency=3, start=c(2007,1))
series_season3_kitchen <- ts(season3_years$kitchen, frequency=3, start=c(2007,1))
series_season3_laundry <- ts(season3_years$laundry_room, frequency=3, start=c(2007,1))
series_season3_wa <- ts(season3_years$water_air, frequency=3, start=c(2007,1))

# plot season 3 series
plot.ts(series_season1_global)
plot.ts(series_season3_kitchen)
plot.ts(series_season3_laundry)
plot.ts(series_season3_wa)

# Season 4
series_season4_global <- ts(season4_years$household_minute_averaged_active_power, frequency=4, start=c(2007,1))
series_season4_kitchen <- ts(season4_years$kitchen, frequency=4, start=c(2007,1))
series_season4_laundry <- ts(season4_years$laundry_room, frequency=4, start=c(2007,1))
series_season4_wa <- ts(season4_years$water_air, frequency=4, start=c(2007,1))

# plot season 4 series
plot.ts(series_season1_global)
plot.ts(series_season4_kitchen)
plot.ts(series_season4_laundry)
plot.ts(series_season4_wa)

#### TSLM Season 1 ####
# global
my_df_ts <- data.frame(temperature = series_season1_global, as.numeric(time(series_season1_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)  + xlab("Time") + ylab("Global Consumption Winter") + ggtitle("TSLM forecast") + theme(plot.title = element_text(hjust = 0.5))


# kitchen
my_df_ts <- data.frame(temperature = series_season1_kitchen, as.numeric(time(series_season1_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_season1_laundry, as.numeric(time(series_season1_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_season1_wa, as.numeric(time(series_season1_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# plot alltogehter
df_ts_s1 <- cbind(Global=series_season1_global, Kitchen=series_season1_kitchen, Laundry=series_season1_laundry,Water_Air=series_season1_wa)
fit <- tslm(df_ts_s1 ~ trend + season)
fcast <- forecast(fit, h=4)
autoplot(fcast)

# Holt Winters

HWplot(series_season1_global) + xlim(2007,2012)


#hw <- HoltWinters(series_winter_days_global)
#plot(hw)

#forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
#plot(hw, forecast)


#### TSLM Season 2 ####
# global
my_df_ts <- data.frame(temperature = series_season2_global, as.numeric(time(series_season2_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_season2_kitchen, as.numeric(time(series_season2_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_season2_laundry, as.numeric(time(series_season2_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_season2_wa, as.numeric(time(series_season2_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# plot alltogehter
df_ts_s2 <- cbind(Global=series_season2_global, Kitchen=series_season2_kitchen, Laundry=series_season2_laundry,Water_Air=series_season2_wa)
fit <- tslm(df_ts_s1 ~ trend + season)
fcast <- forecast(fit, h=4)
autoplot(fcast)

# Holt Winters
hw <- HoltWinters(series_winter_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)



#### TSLM Season 3 ####
# global
my_df_ts <- data.frame(temperature = series_season3_global, as.numeric(time(series_season3_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_season3_kitchen, as.numeric(time(series_season3_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_season3_laundry, as.numeric(time(series_season3_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_season3_wa, as.numeric(time(series_season3_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)

# plot alltogehter
df_ts_s3 <- cbind(Global=series_season3_global, Kitchen=series_season3_kitchen, Laundry=series_season3_laundry,Water_Air=series_season3_wa)
fit <- tslm(df_ts_s1 ~ trend + season)
fcast <- forecast(fit, h=4)
autoplot(fcast)

# Holt Winters
hw <- HoltWinters(series_winter_days_global)
plot(hw)

# forecast
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast)



#### TSLM Season 4 ####
# global
my_df_ts <- data.frame(temperature = series_season4_global, as.numeric(time(series_season4_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# kitchen
my_df_ts <- data.frame(temperature = series_season4_kitchen, as.numeric(time(series_season4_kitchen)))
names(my_df_ts) <- c("kitchen_Consumption", "time")

mymodel <- tslm(kitchen_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# laundry
my_df_ts <- data.frame(temperature = series_season4_laundry, as.numeric(time(series_season4_laundry)))
names(my_df_ts) <- c("laundry_Consumption", "time")

mymodel <- tslm(laundry_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=4)
autoplot(my_fc)

# water & air
my_df_ts <- data.frame(temperature = series_season4_wa, as.numeric(time(series_season4_wa)))
names(my_df_ts) <- c("Water&Air_Consumption", "time")

mymodel <- tslm(Water&Air_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=24)
autoplot(my_fc)




#### Holt Winters season 1 ####
# Global #
hw_season1_global <- HoltWinters(series_season1_global)
plot(hw_season1_global)

forecast <- predict(hw_season1_global, n.ahead = 4, prediction.interval = T, level = 0.95)
plot(hw_season1_global, forecast)

### Kitchen ###
hw_season1_kitchen <- HoltWinters(series_season1_kitchen)
plot(hw_season1_kitchen)

forecast <- predict(hw_season1_kitchen, n.ahead = 4, prediction.interval = T, level = 0.95)
plot(hw_season1_kitchen, forecast)

### Laundry ###
hw_season1_laundry <- HoltWinters(series_season1_laundry)
plot(hw_season1_laundry)

forecast <- predict(hw_season1_laundry, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season1_laundry, forecast)

### Water and Air ###
hw_season1_wa <- HoltWinters(series_season1_wa)
plot(hw_season1_wa)

forecast <- predict(hw_season1_wa, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season1_wa, forecast)

#### Holt Winters season 2 ####
# Global #
hw_season2_global <- HoltWinters(series_season2_global)
plot(hw_season2_global)

# forecast
forecast <- predict(hw_season2_global, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season2_global, forecast)

### Kitchen ###
hw_season2_kitchen <- HoltWinters(series_season2_kitchen)
plot(hw_season2_kitchen)

# forecast
forecast <- predict(hw_season2_kitchen, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season2_kitchen, forecast)

### Laundry ###
hw_season2_laundry <- HoltWinters(series_season2_laundry)
plot(hw_season2_laundry)

# forecast
forecast <- predict(hw_season2_laundry, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season2_laundry, forecast)

### Water and Air ###
hw_season2_wa <- HoltWinters(series_season2_wa)
plot(hw_season2_wa)

# forecast
forecast <- predict(hw_season2_wa, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season2_wa, forecast)


#### Holt Winters season 3 ####
# Global #
hw_season3_global <- HoltWinters(series_season3_global)
plot(hw_season3_global)

# forecast
forecast <- predict(hw_season3_global, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season3_global, forecast)

### Kitchen ###
hw_season3_kitchen <- HoltWinters(series_season3_kitchen)
plot(hw_season3_kitchen)

# forecast
forecast <- predict(hw_season3_kitchen, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season3_kitchen, forecast)

### Laundry ###
hw_season3_laundry <- HoltWinters(series_season3_laundry)
plot(hw_season3_laundry)

# forecast
forecast <- predict(hw_season3_laundry, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season3_laundry, forecast)

### Water and Air ###
hw_season3_wa <- HoltWinters(series_season3_wa)
plot(hw_season3_wa)

# forecast
forecast <- predict(hw_season3_wa, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season3_wa, forecast)

#### Holt Winters season 4 ####
# Global #
hw_season4_global <- HoltWinters(series_season4_global)
plot(hw_season4_global)

# forecast
forecast <- predict(hw_season4_global, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season4_global, forecast)

### Kitchen ###
hw_season4_kitchen <- HoltWinters(series_season4_kitchen)
plot(hw_season4_kitchen)

# forecast
forecast <- predict(hw_season4_kitchen, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season4_kitchen, forecast)

### Laundry ###
hw_season4_laundry <- HoltWinters(series_season4_laundry)
plot(hw_season4_laundry)

# forecast
forecast <- predict(hw_season4_laundry, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season4_laundry, forecast)

### Water and Air ###
hw_season4_wa <- HoltWinters(series_season4_wa)
plot(hw_season4_wa)

# forecast
forecast <- predict(hw_season4_wa, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_season4_wa, forecast)


#### decomposed winters over years ####
series_winters_global_dc <- decompose(series_season1_global)
series_winters_kitchen_dc <- decompose(series_season1_kitchen)
series_winters_laundry_dc <- decompose(series_season1_laundry)
series_winters_wa_dc <- decompose(series_season1_wa)

# plot decomposed
plot(series_winters_global_dc)
plot(series_winters_kitchen_dc)
plot(series_winters_laundry_dc)
plot(series_winters_wa_dc)

