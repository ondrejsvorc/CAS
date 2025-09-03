setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

bike_sharing <- read.csv("bike_sharing.csv", nrows = 2000)
bike_sharing$dteday <- as.Date(bike_sharing$dteday)
bike_sharing$season <- factor(bike_sharing$season, levels = 1:4, labels = c("Winter", "Spring", "Summer", "Fall"))
bike_sharing$weathersit <- factor(bike_sharing$weathersit, levels = 1:4, labels = c("Clear", "Mist", "Light rain", "Heavy rain"))
bike_sharing$weekday <- factor(bike_sharing$weekday, levels = 0:6, labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
bike_sharing$yr <- factor(bike_sharing$yr, levels = 0:1, labels = c("2011", "2012"))
bike_sharing$mnth <- factor(bike_sharing$mnth, levels = 1:12, labels = month.name)
bike_sharing$holiday <- as.logical(bike_sharing$holiday)
bike_sharing$workingday <- as.logical(bike_sharing$workingday)
bike_sharing$hum <- bike_sharing$hum * 100
bike_sharing$windspeed <- bike_sharing$windspeed * 67
bike_sharing$temp <- bike_sharing$temp * (39 - (-8)) + (-8)
bike_sharing$atemp <- bike_sharing$atemp * (50 - (-16)) + (-16)
bike_sharing$datetime <- as.POSIXct(paste(bike_sharing$dteday, bike_sharing$hr), format = "%Y-%m-%d %H")

str(bike_sharing)
head(bike_sharing)