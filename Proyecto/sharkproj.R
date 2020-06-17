###diagrama de dispersion

datostiburones = read.csv("sharkdata.csv")
vectorx=datostiburones$d13C
vectory=datostiburones$d15N

png("sharkposition.png")
ggplot(datostiburones, aes(d13C, d15N, colour = Edad)) + geom_point()+
xlab('Carbono') + 
ylab('NItrógeno') +
ggtitle('Posición isotópica de los tiburones') 
graphics.off()

###MANOVA Y ANOVA

library(car)
library(ggplot2)
library(MASS)
library(WRS)
library(tidyverse)
library(akima)
library(reshape)
library(mvoutlier)
library(dplyr)
library(pastecs)
library(mvnormtest)
library(robustbase)
library(stats)
library(rapportools)

d = read.csv('sharkdata.csv', sep=',', header=TRUE)
view(d)
d$EdadRda<-factor(d$Edad, levels=c("1", "2", "3", "4", "5", "6"), labels = c("uno",
"dos", "tres", "cuatro", "cinco", "seis"))
boxplot(d15N ~ Edad, data=d)
boxplot(d13C ~ Edad, data=d)
grafica1= ggplot(d, aes (d13C, d15N))
grafica1 + geom_point()
combi=cbind(d$d15N,d$d13C)
modelo1 = manova(combi ~ Edad, data=d)
modelo1
summary(modelo1, intercept=TRUE)
summary(modelo1, intercept=TRUE, test="Wilks")
summary(modelo1, intercept=TRUE, test="Hotelling")
summary(modelo1, intercept=TRUE, test="Roy")
summary(modelo1)
summary.aov(modelo1)
anadis=lda(EdadRda ~ d15N + d13C, data=d)
anadis
predict(anadis)
plot(anadis)

anova <- aov(d$d15N ~ d$Edad_Promedio)
summary(anova)

anova <- aov(d$d13C ~ d$Edad_Promedio)
summary(anova)

anova <- aov(d$Edad_Promedio ~ d$d15N)
summary(anova)

anova <- aov(d$Edad_Promedio ~ d$d13C)


###Histogramas

d = read.csv('sharkdata.csv', sep=',', header=TRUE)

cero=d[d$EdadRda==1,]
uno=d[d$EdadRda==2,]
dos=d[d$EdadRda==3,]
tres=d[d$EdadRda==4,]
cuatro=d[d$EdadRda==5,]
cinco=d[d$EdadRda==6,]

filas = 6
columnas = 2

png('histo.png', width = 600, height = 800)
par(mfrow=c(filas, columnas))

histo1=hist(cero$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo2=hist(cero$d13C, col='green', xlim=c(-18, -14),ylim = c(0, 20))
histo3=hist(uno$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo4=hist(uno$d13C, col='green', xlim=c(-18, -14),ylim = c(0, 20))
histo5=hist(dos$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo6=hist(dos$d13C, col='green', xlim=c(-18, -14),ylim = c(0, 20))
histo7=hist(tres$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo8=hist(tres$d13C, col='green',xlim=c(-18, -14),ylim = c(0, 20))
histo9=hist(cuatro$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo10=hist(cuatro$d13C, col='green', xlim=c(-18, -14),ylim = c(0, 20))
histo11=hist(cinco$d15N,  col='green', xlim=c(7, 14), ylim = c(0, 20))
histo12=hist(cinco$d13C, col='green', xlim=c(-18, -14),ylim = c(0, 20))

graphics.off()


filas = 6
columnas = 2

png("histoFR.png", width = 600, height = 800)

par(mfrow=c(filas, columnas))

histo1$counts <- histo1$counts / sum(histo1$counts)
plot(histo1, freq=TRUE, main= "Edad 1", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo2$counts <- histo2$counts / sum(histo2$counts)
plot(histo2, freq=TRUE, main= "Edad 1", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")


histo3$counts <- histo3$counts / sum(histo3$counts)
plot(histo3, freq=TRUE, main= "Edad 2", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo4$counts <- histo4$counts / sum(histo4$counts)
plot(histo4, freq=TRUE, main= "Edad 2", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")


histo5$counts <- histo5$counts / sum(histo5$counts)
plot(histo5, freq=TRUE, main= "Edad 3", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo6$counts <- histo6$counts / sum(histo6$counts)
plot(histo6, freq=TRUE, main= "Edad 3", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")


histo7$counts <- histo7$counts / sum(histo7$counts)
plot(histo7, freq=TRUE, main= "Edad 4", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo8$counts <- histo8$counts / sum(histo8$counts)
plot(histo8, freq=TRUE, main= "Edad 4", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")


histo9$counts <- histo9$counts / sum(histo9$counts)
plot(histo1, freq=TRUE, main= "Edad 5", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo10$counts <- histo10$counts / sum(histo10$counts)
plot(histo10, freq=TRUE, main= "Edad 5", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")

histo11$counts <- histo11$counts / sum(histo11$counts)
plot(histo11, freq=TRUE, main= "Edad 6", xlab= "Nitrógeno", ylab="Frecuencia Relativa", xlim=c(7, 14), ylim=c(0,0.8), col="green")

histo12$counts <- histo12$counts / sum(histo12$counts)
plot(histo12, freq=TRUE, main= "Edad 6", xlab= "Carbono", ylab="Frecuencia Relativa", xlim=c(-18, -14), ylim=c(0,0.8), col="green")


graphics.off()


###Diagramas de Violin

png("violinN.png",, height = 10, width = 20, units = "cm", res = 900)
ggplot(data = d, aes(x = EdadRda, y = d15N)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = EdadRda), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Edad') + 
  ylab('Nitrógeno') +
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red") + 
  theme_minimal()


graphics.off()

png("violinC.png",, height = 10, width = 20, units = "cm", res = 900)
ggplot(data = d, aes(x = EdadRda, y = d13C)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = EdadRda), color = 'black', alpha = 0.8) +
  geom_boxplot(color = 'black', alpha = 0.7) + 
  xlab('Edad') + 
  ylab('Carbono') +
  stat_summary(fun.y=mean, geom="point", shape=18,size=3, color="red") + 
  theme_minimal()


graphics.off()




