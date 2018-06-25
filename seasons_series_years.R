library(forecast)
library(ggplot2)
library(reshape)

seasons_mean <- read.csv("/home/johannes/Documents/Ubiqum/Course4/task2/data/seasons_mean.csv", sep=",",na.strings = c("?", "NA"), fill = T, stringsAsFactors = FALSE)

#### HW function ####
HWplot <- function(seasons_series_global,  n.ahead = 12,  CI = .95,  error.ribbon = 'green', line.size = 1){
  
  hw <- HoltWinters(seasons_series_global)
  
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
    xlab('Time') + ylab('Global Consumption Seasons') + ggtitle("Holt-Winters forecast") + theme(plot.title = element_text(hjust = 0.5))
  return(p)
  
}




#### TS seasons ####
seasons_series_global <- ts(seasons_mean$household_minute_averaged_active_power, frequency=4, start=c(2007,1))
seasons_series_kitchen <- ts(seasons_mean$kitchen, frequency=4, start=c(2007,1))
seasons_series_laundry <- ts(seasons_mean$laundry_room, frequency=4, start=c(2007,1))
seasons_series_wa <- ts(seasons_mean$water_air, frequency=4, start=c(2007,1))

# plot seasons series
plot.ts(seasons_series_global)
plot.ts(seasons_series_kitchen)
plot.ts(seasons_series_laundry)
plot.ts(seasons_series_wa)

# TSLM global ####

my_df_ts <- data.frame(temperature = seasons_series_global, as.numeric(time(seasons_series_global)))
names(my_df_ts) <- c("Global_Consumption", "time")

mymodel <- tslm(Global_Consumption~season+trend,my_df_ts)

my_fc <- forecast(mymodel,h=12)
autoplot(my_fc) + xlab("Time") + ylab("Global Consumption Seasons") + ggtitle("TSLM forecast") + theme(plot.title = element_text(hjust = 0.5))

#### Holt WInters ####
### Global ###

HWplot(seasons_series_global) + xlim(2007,2012)

#hw_global <- HoltWinters(seasons_series_global)
#plot(hw_global)

# forecast
#forecast <- predict(hw_global, n.ahead = 12, prediction.interval = T, level = 0.95)
#plot(hw_global, forecast)

### Kitchen ###
hw_kitchen <- HoltWinters(seasons_series_kitchen)
plot(hw_kitchen)

# forecast
forecast <- predict(hw_kitchen, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_kitchen, forecast)

### Laundry ###
hw_laundry <- HoltWinters(seasons_series_laundry)
plot(hw_laundry)

# forecast
forecast <- predict(hw_laundry, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_laundry, forecast)

### Water and Air ###
hw_wa <- HoltWinters(seasons_series_wa)
plot(hw_wa)

# forecast
forecast <- predict(hw_wa, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw_wa, forecast)

#### decomposed seasons series over years ####
seasons_series_global_dc <- decompose(seasons_series_global)
seasons_series_kitchen_dc <- decompose(seasons_series_kitchen)
seasons_series_laundry_dc <- decompose(seasons_series_laundry)
seasons_series_wa_dc <- decompose(seasons_series_wa)



# plot decomposed seasons series
plot(seasons_series_global_dc)
plot(seasons_series_kitchen_dc)
plot(seasons_series_laundry_dc)
plot(seasons_series_wa_dc)

