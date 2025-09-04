setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rows_3_months <- 2067
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
  plot(data$datetime, data$cnt, type="l", col="steelblue", xlab="Den a měsíc", ylab="Počet jízd", main=title, xaxt="n")
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
# Denní sezónnost, pravidelné cykly (špičky ráno/odpoledne, noční minimum), kalendářní vlivy (pracovní den vs víkend).
# Pracovní dny mají podobný průběh (špičky), víkend má nižší průběh (plošší křivka).
# Intradenní průběh dále zvýrazňuje výkyvy v čase.
plot_cnt_days(get_subset(bike_sharing, days = 14), "Počet jízd (14 dní)")
plot_cnt_days(get_subset(bike_sharing, days = 7), "Počet jízd (7 dní)")
plot_cnt_hours(get_subset(bike_sharing, days = 1), "Počet jízd (pracovní den)")
plot_cnt_hours(get_subset(bike_sharing, days = 2), "Počet jízd (2 pracovní dny)")

# ii)

# Sezónní složka je kontstatní bez ohledu na trend (=aditivní).
cnt.ts <- ts(bike_sharing$cnt, frequency = 24)
cnt.ts.decomposed <- decompose(cnt.ts, type = "additive")
plot(cnt.ts.decomposed)

# Trend se v průběhu 3 měsíců zvedá (od ledna směrem k jaru je průměrný počet jízd vyšší).
# Klouzavý průměr odfiltroval denní sezónnost a odhalil rostoucí trend v počtu jízd, s krátkodobými propady.
# Osa x symbolizuje počet dnů.
library(zoo)
cnt.ts.rm24 <- rollmean(cnt.ts, k = 24, align = "center")
plot(cnt.ts, col = "gray", main = "Počet jízd – klouzavý průměr")
lines(cnt.ts.rm24, col = "steelblue", lwd = 2)

# iii)

compare_aic <- function(...) {
  a <- AIC(...)
  a[order(a$AIC), ]
}

t <- 1:length(bike_sharing$cnt)
model1 <- lm(cnt ~ t + factor(hr), data = bike_sharing)
model2 <- lm(cnt ~ t + factor(hr) + factor(weekday), data = bike_sharing)
model3 <- lm(cnt ~ t + factor(hr) * factor(weekday), data = bike_sharing)
model4 <- lm(cnt ~ t + factor(hr) * factor(weekday) + temp + weathersit, data = bike_sharing)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
compare_aic(model1, model2, model3, model4)
best_model_iii <- model4

# iv)

library(forecast)
model1 <- Arima(cnt.ts, order=c(0,0,0), seasonal=list(order=c(1,1,1), period=24))
model2 <- Arima(cnt.ts, order=c(1,0,0), seasonal=list(order=c(1,1,1), period=24))
model3 <- Arima(cnt.ts, order=c(0,0,1), seasonal=list(order=c(1,1,1), period=24))
model4 <- Arima(cnt.ts, order=c(0,1,1), seasonal=list(order=c(1,1,1), period=24))
model5 <- Arima(cnt.ts, order=c(1,1,0), seasonal=list(order=c(1,1,1), period=24))
model6 <- Arima(cnt.ts, order=c(1,0,1), seasonal=list(order=c(1,0,0), period=24))
model7 <- Arima(cnt.ts, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=24))
model8 <- auto.arima(cnt.ts, seasonal=TRUE, max.p=2, max.q=2, max.P=1, max.Q=1)
compare_aic(model1, model2, model3, model4, model5, model6, model7, model8)
best_model_iv <- model2

# v)

temp.ts <- ts(bike_sharing$temp, frequency = 24)
hum.ts <- ts(bike_sharing$hum, frequency = 24)
windspeed.ts <- ts(bike_sharing$windspeed, frequency = 24)

# 72 lagů = 3 dny dopředu i dozadu.
# cnt × temp: kladná korelace, lag 0 -> čím vyšší teplota, tím více jízd (okamžitý vliv).
# cnt × hum: záporná korelace, lag +1 -> čím vyšší vlhkost, tím méně jízd (zpoždění 1h).
# cnt × windspeed: slabá záporná korelace, lag +3 -> čím silnější vítr, tím méně jízd (zpoždění 3h).
ccf(cnt.ts, temp.ts, lag.max = 72, main = "Kroskorelační funkce mezi počtem jízd a teplotou")
ccf(cnt.ts, hum.ts, lag.max = 72, main = "Kroskorelační funkce mezi počtem jízd a vlhkostí")
ccf(cnt.ts, windspeed.ts, lag.max = 72, main = "Kroskorelační funkce mezi počtem jízd a rychlostí větru")

# vi)

# Exogenní proměnné dle CCF.
temp_lag0 <- stats::lag(temp.ts, 0)
hum_lag1 <- stats::lag(hum.ts, -1)
windspeed_lag3 <- stats::lag(windspeed.ts, -3)

# Oddělení cílové proměnné a matice exogenních proměnných.
data <- cbind(cnt=cnt.ts, temp_lag0, hum_lag1, windspeed_lag3)
data <- data[complete.cases(data), ]
y <- data[, "cnt"]
X <- data[, c("temp_lag0", "hum_lag1", "windspeed_lag3")]

# ARIMAX model (regrese + ARIMA errors).
model <- forecast::auto.arima(y, xreg = X)
summary(model)
forecast::checkresiduals(model)
best_model_vi <- model

# Proměnná windspeed (nevýznamná) -> vliv větru se nepotvrdil.
library(lmtest)
ct <- coeftest(model)
data.frame(
  p_value = ct[c("temp_lag0","hum_lag1","windspeed_lag3"), "Pr(>|z|)"],
  significance = ifelse(ct[c("temp_lag0","hum_lag1","windspeed_lag3"), "Pr(>|z|)"] < 0.05, "significant", "insignificant")
)

acf(resid(best_model_iii), main="ACF residuí modelu ze zadání iii)")
acf(resid(best_model_iv), main="ACF residuí modelu ze zadání iv)")
acf(resid(best_model_vi), main="ACF residuí modelu ze zadání vi)")

# vii)

aligned <- ts.intersect(cnt=cnt.ts, temp_lag0, hum_lag1, windspeed_lag3)
future_X <- tail(aligned[, c("temp_lag0","hum_lag1","windspeed_lag3")], 10)

f_iv <- forecast(best_model_iv, h=10)
f_vi <- forecast(best_model_vi, xreg=future_X, h=10)

par(mfrow=c(1,2))
plot(f_iv, main="Predikce (ARIMA, iv)")
plot(f_vi, main="Predikce (ARIMAX, vi)")
par(mfrow=c(1,1))

# viii)

data.frame(
  model = c("LM (iii)","ARIMA (iv)","ARIMAX (vi)"),
  AIC = c(AIC(best_model_iii), AIC(best_model_iv), AIC(best_model_vi)),
  BIC = c(BIC(best_model_iii), BIC(best_model_iv), BIC(best_model_vi))
)