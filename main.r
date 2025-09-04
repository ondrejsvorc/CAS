setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rows_3_months = 2067
bike_sharing <- read.csv("bike_sharing.csv", nrows = rows_3_months)
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

# i)

plot(
  x = bike_sharing$datetime,
  y = bike_sharing$cnt,
  type = "l",
  col = "steelblue",
  xlab = "Měsíc",
  ylab = "Počet jízd",
  main = "Počet jízd (cnt)"
)

get_subset <- function(data, days) { data[1:(days*24), ] }
plot_cnt_days <- function(data, title) {
  plot(data$datetime, data$cnt, type="l", col="steelblue", xlab="Čas", ylab="Počet jízd", main=title, xaxt="n")
  axis.POSIXct(1, at=seq(from=min(data$datetime), to=max(data$datetime), by="1 day"), format="%d.%m")
  axis.POSIXct(1, at=seq(from=min(data$datetime), to=max(data$datetime), by="6 hours"), labels=FALSE, tcl=-0.3)
  abline(v=seq(from=min(data$datetime), to=max(data$datetime), by="1 day"), col="lightgray", lty="dotted")
  grid(nx=NA, ny=NULL, col="gray", lty="dotted")
}
plot_cnt_hours <- function(data, title) {
  plot(data$datetime, data$cnt, type="l", col="steelblue", xlab="Hodina", ylab="Počet jízd", main=title, xaxt="n")
  axis.POSIXct(1, at=seq(from=min(data$datetime), to=max(data$datetime), by="1 hour"), format="%H:%M", cex.axis=0.7, tcl=-0.3)
  abline(v=seq(from=min(data$datetime), to=max(data$datetime), by="1 hour"), col="lightgray", lty="dotted")
  grid(nx=NA, ny=NULL, col="gray", lty="dotted")
}
# Denní sezónnost, pravidelné cykly (špičky ráno/odpoledne, noční minimum), kalendářní vlivy (pracovní den vs víkend)
# Pracovní dny mají podobný průběh (špičky), víkend má nižší průběh (plošší křivka)
# Intradenní průběh dále zvýrazňuje výkyvy v čase
plot_cnt_days(get_subset(bike_sharing, days = 14), "Počet jízd (14 dní)")
plot_cnt_days(get_subset(bike_sharing, days = 7), "Počet jízd (7 dní)")
plot_cnt_hours(get_subset(bike_sharing, days = 1), "Počet jízd (pracovní den)")
plot_cnt_hours(get_subset(bike_sharing, days = 2), "Počet jízd (2 pracovní dny)")
