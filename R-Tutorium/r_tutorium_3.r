###                             R-Tutorium Nr. 3                             ###

# Aufbau Tutorium

### 1 Wiederholung Daten einlesen + tidyverse + alfred
### 2 Funktionen schreiben
### 3 Effizientes Programmieren
### 4 Apply Funktionen
### 5 Regressionsanalyse
### 6 Optimierer in R (optim)
### 7 Fehlermeldungen

rm(list=ls()) # Bereinigung der R-Umgebung
setwd("C:/Repositories/lasso_oil_price_forcast/R-Tutorium")
getwd()
library("tidyverse")
library("alfred")
library("tsibble")
library("readxl")
library("xts")
source("log_returns.R")

## Wiederholung Daten einlesen + tidyverse + alfred ####

price_data <- read.table("Brent_Oil.txt", header = TRUE, dec =".")
SP500 <- read_excel("SP500.xlsx", col_types = c("date", "numeric"))
Gold  <- read.table("Gold.txt", header = TRUE, dec =".")
head(price_data); tail(price_data)
summary(price_data)
str(price_data)
glimpse(price_data) # glimpse ist eine Alternative zu str
class(SP500)

SP500 <- as_tsibble(SP500, index = date, regular = FALSE)
class(SP500)
View(SP500)


# Ermittel das Datum und Wert der ersten nicht fehlenden Beobachtung in SP500
SP500 %>%
na.omit() %>%
head(n = 1)

# Achtung mit den unterschiedlichen Datumsformaten
price_data$date <- as.Date(price_data$date, format = "%d.%m.%Y")
SP500$date <- as.Date(SP500$date, format = "%Y-%m-%d")
Gold$date <- as.Date(Gold$date, format = "%d.%m.%Y")

# Erstelle Datensatz aus Ölpreisen und SP500
data <- merge(price_data, SP500, by = "date", all = FALSE) # Nur Zeitperiode, wo beide Variablen verfügbar

# Datensatz mit 4 Variablen
#merge(data, Gold, by = "date", all = FALSE) 

as_tsibble(data, index = date) %>%
index_by(ym = ~ yearmonth(.))

# Bestimmung der monatlichen Volatilität mit Hilfe des Variationskoeffizienten
monthly_Vol <- as_tsibble(data, index = date) %>%
index_by(ym = ~ yearmonth(.)) %>%
summarise(
  Brent_Oil_Vol = 100*(sd(Brent_Oil)/mean(Brent_Oil)),
  SP500_Vol = 100*(sd(SP500)/mean(SP500))
  )
monthly_Vol

# dieser Weg behält die tägliche Frequenz und die alten Variablen
as_tsibble(data, index = date) %>%
index_by(ym = ~ yearmonth(.)) %>%
mutate(
  Brent_Oil_Vol = 100*(sd(Brent_Oil)/mean(Brent_Oil)),
  SP500_Vol = 100*(sd(SP500)/mean(SP500))
)

# Gebe die 5 Monate mit der höchsten monatlichen Volatilität im SP500 aus
monthly_Vol %>%
  select(SP500_Vol) %>%
  arrange(desc(SP500_Vol)) %>%
  head(5)

class(monthly_Vol)

# Transformation tsibble zu ts oder xts
monthy_Vol_ts <- as.ts(monthly_Vol)
monthy_Vol_xts <- as.xts(monthy_Vol_ts)

# Laden von Daten aus der FRED-Datenbank mit Hilfe des Alfred-Pakets
rGDP <- get_fred_series("GDPC1")
rGDP_rt <- get_alfred_series("GDPC1") # das alfred Paket kann auch real-time/vintage Daten herunterladen
class(rGDP)
t10y3m <- get_fred_series("T10Y3M")


## 2 Funktionen schreiben ####

# Einfache Funktion mit obligatorischen Argumenten
abw_quad <- function(X,Y){
  Differenz <- (X-Y)^2
  Differenz
}
abw_quad(X = 2, Y = 2)
#abw_quad(X = 5)

# Einfache Funktion mit obligatorischen und optionalen Argumenten
abw_quad_1 <- function(X,Y = 0){
  Differenz <- (X - Y)^2
  Differenz
}

abw_quad_1(X = 5)

# Berechne Veränderung zwischen zwei Perioden
calc_change <- function(x){
  x_change <- x[2:length(x)]-(x[1:(length(x)-1)])
}

c <- calc_change(Gold$price)
(c <- calc_change(Gold$price))

# Berechne prozentuale Änderung zwischen zwei Perioden
calc_percchange <- function(x){
  x_change <- (x[2:length(x)]/(x[1:(length(x)-1)])-1)*100
}

# Berechne annualisierte prozentuale Änderung zwischen zwei Perioden
calc_comp_ann_change <- function(x, freq){
  n_obs_year <- case_when (
# see https://fredhelp.stlouisfed.org/fred/data/understanding-the-data/formulas-calculate-growth-rates/
  freq == "daily"  ~ 260,
  freq == "annual" ~ 1,
  freq == "monthly" ~ 12,
  freq == "quarterly" ~ 4,
  freq == "biweekly" ~ 26,
  freq == "weekly" ~ 52
  )
#print(n_obs_year)
x_comp_ann_change <- ((x[2:length(x)]/x[1:(length(x)-1)])^n_obs_year-1)*100
}

# Wir verlieren die erste Beobachtung durch die Berechnung
infl_oil_ann <- calc_comp_ann_change(price_data$Brent_Oil, freq = "daily")
infl_oil_ann <- cbind.data.frame(date = price_data[-1, 1], infl_oil_ann)
# Man beachte dass durch die auf das Jahr hochgerechnete Inflation 
#auf Basis täglicher Daten die Werte extrem sind (und zudem sehr volatil)

# Alternativer Ansatz, indem wir die erste Beobachtung als fehlend ersetzen
#infl_oil_ann <- calc_comp_ann_change(price_data$Brent_Oil, freq = "daily")
#infl_oil_ann <- cbind.data.frame(date = price_data[, 1], c(NA,infl_oil_ann))

# Zeige Beobachtungen mit Preisrückgängen an
infl_oil <- cbind.data.frame(date = price_data[-1, 1], infl_oil = calc_percchange(price_data$Brent_Oil))
subset(infl_oil, infl_oil < 0)


## 3 Effizientes Programmieren ####

# Allgemeines
# Sie müssen die Schnelligkeit Ihres Codes nur optimieren, wenn die
# Berechnungen für Ihre Zwecke zu langsam sind!

t <- 10000
system.time(rnorm(t))

#install.packages("microbenchmark")
library(microbenchmark)

Summierung <- function(Zahlen_Vektor){
  for(i in 1:length(Zahlen_Vektor)){
    if(i>=2){
      Summe <- Summe + i
    }else{
      Summe <- i
    }
  }
  Summe
}

Summierung(c(1:t))
sum(1:t)

microbenchmark(sum(1:t), Summierung(1:t))


## 4 Apply Funktionen - weitere Anwendungsbeispiele ####
fred_md <- read_csv("fred-md.csv") 
fred_md <- fred_md[-1,] # erste Spalte enthält empfohlene Transformationscodes (siehe Papier)
fred_md <- na.omit(fred_md) 
# Achtung: hier gehen sehr viele Beobachtungen verloren und dient hier nur zur Vereinfachung

# Spaltenweiser Mittelwert 
summary(fred_md)
fred_md_means <- apply(fred_md[,-1], MARGIN = 2, mean) # Erste Spalte enthält Datum

# Spaltenweise Standardabweichung
fred_md_sd <- apply(fred_md[,-1], MARGIN = 2, sd)

# lapply
asset_list <- list(
  Gold      = Gold$price,
  SP500     = SP500$SP500,
  Brent_Oil = price_data$Brent_Oil
  )
log_returns_assets <- lapply(asset_list, log_function)

plot(log_returns_assets$Gold, type = "l")
plot(log_returns_assets$SP500, type = "l")
plot(log_returns_assets$Brent_Oil, type = "l")



## 5 Regressionsanalyse ####

Gold$date <- as.Date(Gold$date, format = "%d.%m.%Y")
data_reg <- merge(SP500, Gold, by = "date", all = FALSE)
colnames(data_reg)
colnames(data_reg)[3] = "Gold"
my_regression <- lm(SP500 ~ Gold, data = data_reg)
(sreg1 <- summary(my_regression))

# Regressionskoeffizienten

# t-Statistik
(sreg1$coef[2]-0)/sreg1$coef[4]

# 95% Konfidenzintervall
sreg1$coef[2]-qt(0.975, length(sreg1$residuals)-2)*sreg1$coef[4]
sreg1$coef[2]+qt(0.975, length(sreg1$residuals)-2)*sreg1$coef[4]

# Linearer Hypothesentest mit mehr als einem Koeffiziente
library(car)
linearHypothesis(lm(SP500~Gold, data = data_reg), c("(Intercept) = 0", "Gold = 1"))

# Sind die Residuen normalverteilt
summary(sreg1$residuals)
hist(sreg1$resid, freq = FALSE)
lines(seq(-2000,2000,0.2), dnorm(seq(-2000,2000,0.2), mean = mean(sreg1$resid), sd= sd(sreg1$resid)))

library(fBasics)
jarqueberaTest(sreg1$resid)

# Sind die Residuen frei von Autokorrelation
plot(sreg1$resid, type="l")
acf(sreg1$resid)
pacf(sreg1$resid)
library(lmtest)
bgtest(my_regression, order = 1)

# Regression mit Renditen statt Preisen
SP500_ret <- log_function(data_reg[,2])
Gold_ret <- log_function(data_reg[,3])
length(SP500_ret)
length(Gold_ret)
my_regression_ret <- lm(SP500_ret ~ Gold_ret)
summary(my_regression_ret)
# Welches Problem liegt bei der Regression in Niveaus (Preisen) vor?

ggplot(data_reg, aes ( x = date, y = Gold)) +
geom_line()
p_Gold <- ggplot(data_reg, aes ( x = date, y = Gold)) +
geom_line()
p_SP500 <- ggplot(data_reg, aes ( x = date, y = SP500)) +
geom_line()

library(ggpubr)
theme_set(theme_pubr())
fig <- ggarrange(
        p_Gold, p_SP500,
        ncol = 1, nrow = 2
        )
fig

# Exportiere die Abbildung als png Datei
png(filename = "Macro_try_arounds.png", width = 1200, height = 700)
fig
dev.off()

# Alternativer Export mit ggexport
ggexport(p_Gold, filename = "test.png", width = 1200, height = 700)
ggexport(p_Gold, p_SP500, filename = "test.pdf",nrow = 2, ncol = 1)


## 6 Optimierer und Maximum Likelihood ####

NormalDistributionPrices <- function(data, par){
  -sum(log(1/(sqrt(2 * pi * par[2]^2))) - ((data - par[1])^2)/(2 * par[2]^2))
  #das Minus macht aus dem Minimierungsproblem ein Maximierungsproblem
}

# Maximum-Likelihood-Schätzer zur Schätzung der Parameter der Normalverteilung
ML_estimator <- optim(
  par = c(0,1),
  fn = NormalDistributionPrices,
  method = "L-BFGS-B",
  hessian = TRUE,
  data = Gold$price,
  lower = c(-0.2, 0)
  )

ML_estimator

overview_ND <- matrix(
  c(ML_estimator$par[1], mean(Gold$price),
  ML_estimator$par[2], sd(Gold$price)),
  nrow = 2, ncol = 2, byrow = TRUE
  )

colnames(overview_ND) <- c("L-BFGS-B", "analytical estimate")
rownames(overview_ND) <- c("mu", "sigma")
overview_ND

# OLS/Kleinste Quadrate wählt die Schätzer, die die Residuenquadratsumme minimieren
# Das Problem kann alternativ mit ML gelöst werden
SS_min = function(data,par){
  b0=par[1]
  b1=par[2]
  loss = with(data, sum((b0+b1*x - y)^2))
  return(loss)
  }
dat = data.frame(y = SP500_ret, x = Gold_ret)
res1a = optim(par=c(0,0),SS_min, data=dat)
cbind('lm()' = round(coef(my_regression_ret),5), 'MLE_SS_min' = round(res1a$par,5))
# Maximum-Likelihood-Schätzung liefert identische Parameter wie lm()/OLS
# Nichtsdestotrotz ist OLS hier klar bevorzugt, da ML verzerrte Standardfehler liefert (hier nicht gezeigt)
# und die geschätzteb Parameter auch von den Startwerten der Optimierung abhängen



## 7 Fehlermeldungen in R ####

# Im nachfolgenden werden absichtlich fehlerhafte Codes aufgeführt, damit Sie
# sich mit den Fehlermeldungen vertraut machen können.


# matrix(data = NA)    # Klammer zu viel

# matrixx(data = NA)    # falscher Funktionsname

# c(4,3,)               # fehlendes Argument

# matrix(data = NA 1)   # Komma und ncol fehlt

# matrix(data = NA;1)   # Semikolon statt Komma


# nicht ganz eindeutige Fehlermeldungen
# x <- 4
# y <- 5
# if(x = y){
#   print("x and y are identical!")
# }
# # else Bedingung fehlt und == muss verwendet werden

# sd()               # obligatorisches Element fehlt

# X <- c(1,2,3,4)
# X[1,]              # falsche Anzahl von Dimensionen (Vektor wie Matrix behand.)

# X <- g + 2         # g nicht definiert

# storage_matrix <- matrix(data = 1:6, nrow = 6, ncol = 1)
# storage_matrix[4,2] # Indizierung au?erhalb der Grenzen (gibt keine 2. Spalte)



