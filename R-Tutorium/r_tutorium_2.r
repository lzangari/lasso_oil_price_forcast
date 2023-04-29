## R-Tutorium Nr. 2

## Aufbau Tutorium
## 
## Funktionen und Pakete
## Eigene Funktionen
## Plots
## Schleifen vermeiden und apply
## FRED Datenbank

## 

rm(list=ls()) # Bereinigung der R-Umgebung

help(lm)

## Daten erzeugen
?rnorm
x <- rnorm(500,mean=10,sd=2)
y <- rnorm(500)

## Modell implementieren
model1 <- lm(y~x)
summary(model1)
model1$coefficients
model1$

## Pakete
# install.packages("dynlm")
library(dynlm)
dynlm(y~x)

# install.packages("urca")
library(urca)
?ur.df
ur_test <- ur.df(x)
summary(ur_test)
??urca

# install.packages("beepr)
library(beepr)
beep(2)

## Momente 

mean_x <- mean(x)
sd_x <- sd(x)
variance_x <- var(x)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
skew_x <- skewness(x,method="moment")
kurt_x <- kurtosis(x,method="moment")
momente <- cbind(mean_x,variance_x,skew_x,kurt_x)
momente


## Schreiben eigener Funktionen
meine_erste_Funktion <- function(Vektor,Ergebnis,Bezeichnung){
  if(is.vector(Vektor)==T){
  Summe <- sum(Vektor)
  if(Ergebnis==T){
    return(paste(Bezeichnung,Summe))}
  else{
    return("Kein Ergebnis angezeigt")
    }
  }
}
d <- 1:10
meine_erste_Funktion(Vektor=d,Ergebnis = T,Bezeichnung="Die Summe des Vektors ist")



meine_erste_Funktion <- function(Vektor,Ergebnis,Bezeichnung){
  if(is.vector(Vektor)==T){
    Summe <- sum(Vektor)
    if(Ergebnis==T){
      return(paste(Bezeichnung,Summe))}
    else{
      return("Kein Ergebnis angezeigt")
    }
  }else(print("Die für die Funktion verwendeten Daten müssen vom Datentyp Vektor sein"))
}
d2 <- data.frame(x,y)
meine_erste_Funktion(Vektor=d2,Ergebnis = T,Bezeichnung="Die Summe des Vektors ist")



## Plots erstellen
## Scatter
plot(x)

## Boxplot
boxplot(x)

## Histogramm
hist(x)

## Dichte
plot(density(x))

hist(x,breaks=20,freq = F)
lines(density(x))

## Zeitreihenplots
xts <- ts(x,start=c(1947,1),frequency = 12)
xts
class(xts)
plot.ts(xts)

yts <- ts(y,start=c(1947,1),frequency = 12)
plot.ts(cbind(xts,yts))
plot.ts(cbind(xts,yts),plot.type="single",col=c("red","blue"))



xts2 <- cbind.data.frame(as.Date(time(xts), format = "%d.%m.%Y"),
                         as.numeric(xts))

colnames(xts2) <- c("date","x")
plot(xts2,type="l",xlab="Zeit",ylab="x",main="x-Werte",lwd=0.5)
abline(h=10)
abline(v=as.Date("01.08.1980", format="%d.%m.%Y"), col ="blue",lwd=5)


## Q-Q-Plots
qqnorm(x)
qqline(x)

## Plots positionieren
par(mfrow=c(2,1))
plot(x)
plot(y)
par(mfrow=c(1,1))

## Plots exportieren
pdf(file="C:/.../.../.../.pdf")
plot(x)
dev.off()

## ggplot
# install.packages("ggplot2")
library(ggplot2)
ggplot(xts2, aes(x=date,y=x))+
  geom_line(color="green")+
  theme_minimal()+
  labs(x="",y="x",title="x-Plot")+
  theme(plot.title = element_text(hjust=0.5, size=10, face="bold"))





my_matrix = matrix(data=rep(rnorm(10000),10000), nrow = 10000, ncol=10000)

# Option 1 - apply anwenden
Test1 = apply(my_matrix, 1, sum)

# Option 2 - Aufwendiger mit for Schleife
storage_matrix = matrix(data=NA, nrow = length(my_matrix[1,]),
                        ncol=1)
for(i in 1:length(my_matrix[1,])){
  storage_matrix[i] = sum(my_matrix[i,])
}


## Zeit messen
start_time <- Sys.time()
Test1 = apply(my_matrix, 1, sum)
end_time <- Sys.time()
Zeit1 <- end_time-start_time

start_time <- Sys.time()
storage_matrix = matrix(data=NA, nrow = length(my_matrix[1,]),
                        ncol=1)
for(i in 1:length(my_matrix[1,])){
  storage_matrix[i] = sum(my_matrix[i,])
}
end_time <- Sys.time()
Zeit2 <- end_time-start_time




# lapply
my_vec_1 = 1:25
my_vec_2 = 10:190
my_vec_3 = 1:3
my_list = list(my_vec_1, my_vec_2, my_vec_3)
lapply(my_list, sum)

