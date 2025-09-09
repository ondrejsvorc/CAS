setwd(if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) dirname(rstudioapi::getActiveDocumentContext()$path) else getwd())

# Transformace datasetu, primárně denormalizace vybraných proměnných a přetypování proměnných na správné datové typy.
# Některé proměnné byly autorem normalizovány pro účely strojového učení (např. teplota byla reprezentována intervalem <0, 1>).
# Přidání dodatečné proměnné, která je spojením dne a měřené hodiny výpůjčky kola pro čitelnější časovou osu u grafů.
rows_3_months <- 2067
bike_sharing <- read.csv("bike_sharing.csv", nrows = rows_3_months)
bike_sharing$dteday <- as.Date(bike_sharing$dteday)
bike_sharing$season <- factor(bike_sharing$season, levels = 1:4, labels = c("Zima", "Jaro", "Léto", "Podzim"))
bike_sharing$weathersit <- factor(bike_sharing$weathersit, levels = 1:4, labels = c("Jasno", "Oblačno", "Slabý déšť", "Silný déšť"))
bike_sharing$weekday <- factor(bike_sharing$weekday, levels = 0:6, labels = c("Neděle", "Pondělí", "Úterý", "Středa", "Čtvrtek", "Pátek", "Sobota"))
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

# Pozvolna rostoucí lineární trend (lidé začínají více jezdit, pravděpodobně kvůli zlepšujícímu se počasí).
plot(x = bike_sharing$datetime, y = bike_sharing$cnt, type = "l", col = "steelblue", xlab = "Měsíc", ylab = "Počet jízd", main = "Počet jízd (cnt)")

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

# V pracovní dny je jasně vidět ranní a odpolední dopravní špička, zatímco o víkendu je průběh plošší a maximum nastává spíše odpoledne.
# Interpretace může být taková, že lidé chodí v pracovní dny ráno do práce/školy a odpoledne ze školy/práce, tak si kolo v tento čas vypůjčují nejvíce.
# O víkendech si zase lidé obecně rádi přispí, takže nárust výpůjček začíná až v pozdějších dopoledních a odpoledních hodinách, pravděpodobně za účelem aktivního odpočinku.
# Naše inference vyplívá z datové sady pouze nepřímo. O konkrétních aktivitách uživatelů informace nemáme. Jedná se pouze o jedno z možných vysvětlení.
hourly_cnt_means_by_day <- aggregate(cnt ~ hr + workingday, data = bike_sharing, FUN = mean)
working_days <- subset(hourly_cnt_means_by_day, workingday == TRUE)
weekends <- subset(hourly_cnt_means_by_day, workingday == FALSE)
plot(x = working_days$hr, y = working_days$cnt, type="l", col="steelblue", xlab="Hodina", ylab="Průměrný počet jízd", main="Průměrný denní průběh počtu jízd", xaxt="n")
lines(weekends$hr, weekends$cnt, lty=2, col="darkred")
axis(1, at=0:23, labels=0:23, cex.axis=0.8, tcl=-0.3)
abline(v=0:23, col="lightgray", lty="dotted")
grid(nx=NA, ny=NULL, col="gray", lty="dotted")
legend("topleft", c("Pracovní den","Víkend"), lty=c(1,2), col=c("steelblue","darkred"), bty="n")

# Nejvíce jízd je během pracovních dnů, pátek mírně vyčnívá, víkendy jsou slabší.
# Špičky jsou kolem 8:00 a 17:00, přičemž 18:00-4:00 počet jízd klesá až do úplných minim, a od 5:00 počet jízd zase vzrůstá.
# Čím je lepší počasí, tím je více jízd.
boxplot(cnt ~ weekday, data=bike_sharing, xlab="Den v týdnu", ylab="Počet jízd", main="Rozdělení počtu jízd podle dne v týdnu")
boxplot(cnt ~ hr, data=bike_sharing, xlab="Hodina v týdnu", ylab="Počet jízd", main="Rozdělení počtu jízd podle hodiny dne")
boxplot(cnt ~ weathersit, data=bike_sharing, xlab="Počasí", ylab="Počet jízd", main="Rozdělení počtu jízd podle počasí")

# ii)

# Trend se v průběhu 3 měsíců zvedá (od ledna směrem k jaru je průměrný počet jízd vyšší - to odpovídá nárustu jízd při lepším počasí).
# Sezónní složka je kontstatní bez ohledu na trend (=aditivní).
cnt.ts <- ts(bike_sharing$cnt, frequency = 24)
cnt.ts.decomposed <- decompose(cnt.ts, type = "additive")
plot(cnt.ts.decomposed)

# Klouzavý průměr (24h) odfiltruje denní cyklus a ukáže čistější trend v datech.
# Osa x symbolizuje počet dnů.
library(zoo)
cnt.ts.rm24 <- rollmean(cnt.ts, k = 24, align = "center")
plot(cnt.ts, col = "gray", xlab = "Den", ylab = "Počet jízd", main = "Počet jízd s klouzavým průměrem")
lines(cnt.ts.rm24, col = "steelblue", lwd = 2)
legend("topleft", legend = c("Původní řada", "Klouzavý průměr (24h)"), col = c("gray", "steelblue"), lty = 1, lwd = c(1, 2), bty = "n")

# iii)

compare_aic <- function(...) {
  a <- AIC(...)
  a[order(a$AIC), ]
}

t <- 1:length(bike_sharing$cnt)
model1 <- lm(cnt ~ t + factor(hr), data = bike_sharing); summary(model1)
model2 <- lm(cnt ~ t + factor(hr) + factor(weekday), data = bike_sharing); summary(model2)
model3 <- lm(cnt ~ t + factor(hr) * factor(weekday), data = bike_sharing); summary(model3)
model4 <- lm(cnt ~ t + factor(hr) * factor(weekday) + temp + weathersit, data = bike_sharing); summary(model4)
compare_aic(model1, model2, model3, model4)
# cnt_t = β0 + β1·t + f(hr, weekday) + β2·temp_t + g(weathersit) + ε_t
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
# SARIMA(p,d,q)(P,D,Q)[s] = SARIMA(1,0,0)(1,1,1)[24]
# p=1 : Dnešní počet jízd souvisí s počtem jízd včera (autoregrese).
# d=0 : Trend není nutné odstraňovat zvlášť (počet jízd neroste pořád dokola nahoru - stačí odstranit denní cyklus).
# q=0 : Chyby z minulých předpovědí ignoruju, vystačím si s autoregresí a sezónností.
# P=1 : Dnešní počet jízd v danou hodinu souvisí s počtem jízd v danou hodinu včera (sezónní autoregrese).
# D=1 : Porovnáváme hodnoty s těmi před 24h -> odstraníme denní cyklus (sezónní rozdílování).
# Q=1 : Model zohlední, jak moc se spletl ve stejné hodině včera (učení z chyb).
# s=24: Cyklus je 24 hodin.
best_model_iv <- model2 

# v)

temp.ts <- ts(bike_sharing$temp, frequency = 24)
hum.ts <- ts(bike_sharing$hum, frequency = 24)
windspeed.ts <- ts(bike_sharing$windspeed, frequency = 24)

# Funkce pro určení lagu s nejvyšší absolutní korelací.
# Lag ukazuje, jestli se počet jízd mění hned, nebo až s nějakým zpožděním.
# Př.: (cnt × temp) lag +1 = dívám se, jestli teplota před hodinou souvisí s počtem jízd teď.
# Př.: (cnt × temp) lag +0 = porovnávám teplotu a počet jízd ve stejnou hodinu.
# Př.: (cnt × temp) lag -1 = dívám se, jestli jízdy teď souvisí s teplotou za hodinu.
determine_lag <- function(ccf_result) {
  correlations <- as.numeric(ccf_result$acf)
  lags <- ccf_result$lag
  max_idx <- which.max(abs(correlations))
  best_lag <- lags[max_idx]
  best_correlation <- correlations[max_idx]
  
  target_lags <- c(-3, -2, -1, 0, 1, 2, 3)
  rounded_lag <- target_lags[which.min(abs(target_lags - best_lag))]
  rounded_correlation <- round(best_correlation, 2)
  
  abs_corr <- abs(rounded_correlation)
  strength <- if (abs_corr < 0.1) "velmi slabá" else if (abs_corr < 0.3) "slabá" else if (abs_corr < 0.5) "střední" else "silná"
  direction <- if (rounded_correlation > 0) "pozitivní" else "negativní"
  interpretation <- paste(strength, direction, "korelace")
  
  return(c(lag = rounded_lag, correlation = rounded_correlation, interpretation = interpretation))
}
# 72 lagů = 3 dny dopředu i dozadu.
# cnt × temp: kladná korelace, lag 0 -> okamžitý efekt teploty na počet jízd.
# cnt × hum: záporná korelace, lag 0 -> okamžitý negativní efekt vlhkosti.
# cnt × windspeed: slabá záporná korelace, lag +1 -> vítr před hodinou koreluje s menším počtem jízd nyní.
print(determine_lag(ccf(cnt.ts, temp.ts, lag.max = 72, main = "Teplota")))
print(determine_lag(ccf(cnt.ts, hum.ts, lag.max = 72, main = "Vlhkost")))
print(determine_lag(ccf(cnt.ts, windspeed.ts, lag.max = 72, main = "Rychlost větru")))

# vi)

# Exogenní proměnné dle CCF.
# Exogenni = nezávislé proměnné, které ovlivňují časovou řadu, ale nejsou jí samotnou.
# Počasí ovlivňuje počet jízd, ale samotný počet jízd nemění počasí.
temp_lag0 <- lag(temp.ts, k = 0)
hum_lag0 <- lag(hum.ts, k = 0)
windspeed_lag1 <- lag(windspeed.ts, k = 1)
weather_variables <- c("temp_lag0", "hum_lag0", "windspeed_lag1")

# Oddělení cílové proměnné a exogenních proměnných.
data <- cbind(cnt=cnt.ts, temp_lag0, hum_lag0, windspeed_lag1)
data <- data[complete.cases(data), ]
y <- data[, "cnt"]
X <- data[, weather_variables]

# ARIMAX model (regrese + ARIMA errors).
model <- auto.arima(y, xreg = X, seasonal=TRUE)
summary(model)
checkresiduals(model)
best_model_vi <- model

# Proměnná windspeed (nevýznamná) -> vliv větru se nepotvrdil.
library(lmtest)
ct <- coeftest(model)
data.frame(
  p_value = ct[weather_variables, "Pr(>|z|)"],
  significance = ifelse(ct[weather_variables, "Pr(>|z|)"] < 0.05, "významná", "nevýznamná")
)

acf(resid(best_model_iii), main="ACF residuí modelu ze zadání iii)")
acf(resid(best_model_iv), main="ACF residuí modelu ze zadání iv)")
acf(resid(best_model_vi), main="ACF residuí modelu ze zadání vi)")

# vii)

aligned <- ts.intersect(cnt=cnt.ts, temp_lag0, hum_lag0, windspeed_lag1)
future_X <- tail(aligned[, weather_variables], 10)
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