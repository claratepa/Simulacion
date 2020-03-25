library(testit)

inicio<-function(n,k){
    originales <- rnorm(k)
    cumulos <- originales - min(originales) + 1
    cumulos <- round(n * cumulos / sum(cumulos))
    diferencia <- n - sum(cumulos)
    if (diferencia > 0) {
      for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
      }
    } else if (diferencia < 0) {
      for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
          cumulos[p] <- cumulos[p] - 1
        }
      }
    }
    return(cumulos)

}
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
  return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}
unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

particulas<-c(100000,1000000,10000000)
aglomerados<-c(100,1000,10000)
replicas<-30
noparalelo<-data.frame()

for(k in aglomerados){
  for(n in particulas){
    for(replica in 1:replicas){ 
    tiempo<-0
    tinicial<-Sys.time()
    cumulos<-inicio(n,k)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    assert(sum(cumulos) == n)
    c <- median(cumulos) # tamanio critico de cumulos
    d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    duracion <- 5
    
    for (paso in 1:duracion) {
      assert(sum(cumulos) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de rotura
        urna <- freq[i,]
        if (urna$tam > 1) { # no tiene caso romper si no se puede
          cumulos <- c(cumulos, romperse(urna$tam, urna$num))
        } else {
          cumulos <- c(cumulos, rep(1, urna$num))
        }
      }
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      freq <- as.data.frame(table(cumulos)) # actualizar urnas
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de union
        urna <- freq[i,]
        cumulos <- c(cumulos, unirse(urna$tam, urna$num))
      }
      assert(sum(abs(cumulos)) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      juntarse <- -cumulos[cumulos < 0]
      cumulos <- cumulos[cumulos > 0]
      assert(sum(cumulos) + sum(juntarse) == n)
      nt <- length(juntarse)
      if (nt > 0) {
        if (nt > 1) {
          juntarse <- sample(juntarse)
          for (i in 1:floor(nt / 2) ) {
            cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
          }
        }
        if (nt %% 2 == 1) {
          cumulos <- c(cumulos, juntarse[nt])
        }
      }
      assert(sum(cumulos) == n)
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
    }
    tfinal<-Sys.time()
    tiempo<-tfinal-tinicial
    noparalelo<-data.frame(rbind(noparalelo,c(replica,k,n,tiempo)))  
 }   
}
}
names(noparalelo)<-c("Replica","Cumulos","Particulas","Tiempo")
noparalelo$Estado <- "NP"
########################################################################
library(parallel)
library(testit)

inicio<-function(n,k){
  originales <- rnorm(k)
  cumulos <- originales - min(originales) + 1
  cumulos <- round(n * cumulos / sum(cumulos))
  diferencia <- n - sum(cumulos)
  if (diferencia > 0) {
    for (i in 1:diferencia) {
      p <- sample(1:k, 1)
      cumulos[p] <- cumulos[p] + 1
    }
  } else if (diferencia < 0) {
    for (i in 1:-diferencia) {
      p <- sample(1:k, 1)
      if (cumulos[p] > 1) {
        cumulos[p] <- cumulos[p] - 1
      }
    }
  }
  return(cumulos)
}
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
  return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}
unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}
etapa1<-function(i){
  urna <- freq[i,]
  if (urna$tam > 1) { # no tiene caso romper si no se puede
    cumulos <- romperse(urna$tam, urna$num)
    return(cumulos)
  }
  else {
    cumulos <- rep(1, urna$num)
    return(cumulos)
  } 
} 
etapa2<-function(i){
  urna <- freq[i,]
  cumulos <- unirse(urna$tam, urna$num)
  return(cumulos)
}

particulas<-c(100000,1000000, 10000000)
aglomerados<-c(100,1000,10000)
replicas<-30
paralelo<-data.frame()

cluster <- makeCluster(detectCores() - 1)

for(k in aglomerados){
  for(n in particulas){
    for(replica in 1:replicas){
      tiempo<-0
      tinicial<-Sys.time()
      cumulos<-inicio(n,k)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      assert(sum(cumulos) == n)
      c <- median(cumulos) # tamanio critico de cumulos
      d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      duracion <- 5
      for (paso in 1:duracion) {
        clusterExport(cluster, "freq")
        clusterExport(cluster, "romperse")
        clusterExport(cluster, "rotura")
        clusterExport(cluster, "cumulos")
        clusterExport(cluster, "c")
        clusterExport(cluster, "d")
        clusterExport(cluster,"assert")
        clusterExport(cluster, "union")
        clusterExport(cluster,"unirse")
       
        assert(sum(cumulos) == n)
        cumulos <- (parSapply(cluster, 1:dim(freq)[1], etapa1))  
        cumulos <- unlist(cumulos)
        assert(sum(abs(cumulos)) == n)
        assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
        freq <- as.data.frame(table(cumulos)) # actualizar urnas
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)
        clusterExport(cluster, "freq")
        cumulos <- (parSapply(cluster, 1:dim(freq)[1], etapa2))  
        cumulos <- unlist(cumulos)
        juntarse <- -cumulos[cumulos < 0]
        cumulos <- cumulos[cumulos > 0]
        assert(sum(cumulos) + sum(juntarse) == n)
        nt <- length(juntarse)
        if (nt > 0) {
          if (nt > 1) {
            juntarse <- sample(juntarse)
            for (i in 1:floor(nt / 2) ) {
              cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
            }
          }
          if (nt %% 2 == 1) {
            cumulos <- c(cumulos, juntarse[nt])
          }
        }
        assert(sum(cumulos) == n)
        freq <- as.data.frame(table(cumulos))
        names(freq) <- c("tam", "num")
        freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
        assert(sum(freq$num * freq$tam) == n)
      }
      tfinal<-Sys.time()
      tiempo<-tfinal-tinicial
      paralelo<-data.frame(rbind(paralelo,c(replica,k,n,tiempo)))
    }
  }
}
stopCluster(cluster)
names(paralelo)<-c("Replica","Cumulos","Particulas","Tiempo")
paralelo$Estado <- "P"
todo<-data.frame()
todo<-data.frame(rbind(todo,c(paralelo, noparalelo)))
names(todo)<-c("Replica","Cumulos","Particulas","Tiempo","Estado","Cumulos","Particulas","Tiempo","Estado"
)

h <- shapiro.test(noparalelo$Tiempo)
h[[2]] < 0.05
h1 <- shapiro.test(paralelo$Tiempo)
h1[[2]] < 0.05

t.test(noparalelo$Tiempo, paralelo$Tiempo, paired = TRUE, alternative = "two.sided")

png("cumnopar.png", width = 12, height = 12,
     units = "cm", res = 600, pointsize = 10)
boxplot(Tiempo~Cumulos,
data=noparalelo,
boxwex=0.3,
xlab="Cantidad de C\u00FAmulos",
ylab="Tiempo (s)",
col=rainbow(3, alpha=0.2),
border = rainbow(3, v=0.6)
)
dev.off() 

png("partnopar.png", width = 12, height = 12,
     units = "cm", res = 600, pointsize = 10)
boxplot(Tiempo~Particulas,
data=noparalelo,
boxwex=0.3,
xlab="Cantidad de Part\u00edculas",
ylab="Tiempo (s)",
col=topo.colors(3, alpha=0.2),
border = topo.colors(3)
)
dev.off() 

png("cumpar.png", width = 12, height = 12,
     units = "cm", res = 600, pointsize = 10)
boxplot(Tiempo~Cumulos,
data=paralelo,
boxwex=0.3,
xlab="Cantidad de C\u00FAmulos",
ylab="Tiempo (s)",
col=rainbow(3, alpha=0.2),
border = rainbow(3, v=0.6)
)
dev.off() 

png("partpar.png", width = 12, height = 12,
     units = "cm", res = 600, pointsize = 10)
boxplot(Tiempo~Particulas,
data=paralelo,
boxwex=0.3,
xlab="Cantidad de Part\u00edculas",
ylab="Tiempo (s)",
col=topo.colors(3, alpha=0.2),
border = topo.colors(3)
)
dev.off() 
