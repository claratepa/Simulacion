library(parallel)

binario <- function(d, l) {
  b <- rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

paralelizar <- function(i){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  falsos <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    falsos[i] <- resultado
  }
    r= min(decimal(falsos,n),k)
    return(r==d)
}

modelos <- read.csv("digitos.csv", sep=" ", header=FALSE, stringsAsFactors=F)
mn <- c(.995,.995,.99,.008,.01,.001,.99,.99,.003,.5)
mg <- c(.92,.993,.005,.93,.008,.99,.07,.98,.002,.5)
mb <- c(.002,.001,.991,.97,.98,.08,.01,.97,.001,.5)

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

cluster <- makeCluster(detectCores() - 1)
data12 <- data.frame()
tamano <- 1000
repeticiones <- 60
for(h in 1:length(mn)){
  
  modelos[modelos=='n'] <- mn[h]
  modelos[modelos=='g'] <- mg[h]
  modelos[modelos=='b'] <- mb[h]
  ###
  
  for (t in 1:5000) { # entrenamiento
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,]
    correcto <- binario(d, n)
    for (i in 1:n) {
      w <- neuronas[i,]
      deseada <- correcto[i]
      resultado <- sum(w * pixeles) >= 0
      if (deseada != resultado) {
        ajuste <- tasa * (deseada - resultado)
        tasa <- tranqui * tasa
        neuronas[i,] <- w + ajuste * pixeles
      }
    }
  }
  
  for(j in 1:length(tamano)){
    for(repetir in 1:repeticiones){
      nuevo <- c()
      verdaderos <- data.frame
      datos <- data.frame()
      entrena <- tamano[j]
      clusterExport(cluster, "paralelizar")
      clusterExport(cluster, "modelos")
      clusterExport(cluster, "binario")
      clusterExport(cluster, "decimal")
      clusterExport(cluster, "neuronas")
      clusterExport(cluster, "tope")
      clusterExport(cluster, "dim")
      clusterExport(cluster, c("n","verdaderos","k"))
      datos <- parSapply(cluster, 1:entrena, paralelizar)
      datos <- sum(datos)/entrena
      data12 <- rbind(data12,c(repetir, datos, h))
    }
  }
}

stopCluster(cluster)
colnames(data12)= c("Repeticion","Porcentaje","Combinacion")

library('ggplot2')
png(paste("pr12sim.png", sep=""), width=1600, height=800)
ggplot(data=data12,aes(x=as.factor(Combinacion),y=Porcentaje,fill=as.factor(Combinacion)))+
  geom_boxplot()+
  ylab("Porcentaje (%)")+xlab("Combinaciones")+
  scale_fill_discrete(name="Combinaciones")+  
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=30),
        axis.text.x = element_text(size=30, hjust = 1),
        axis.text.y = element_text(size=30),
        plot.title = element_text(size=38))
graphics.off()

table(data12$Combinacion)
aggregate(Porcentaje ~ Combinacion, data = data12, FUN = mean)
aggregate(Porcentaje ~ Combinacion, data = data12, FUN = sd)

require(nortest)
by(data = data12,INDICES = data12$Combinacion,FUN = function(x){ lillie.test(x$Porcentaje)})
anova <- aov(data12$Porcentaje ~ data12$Combinacion)
summary(anova)
