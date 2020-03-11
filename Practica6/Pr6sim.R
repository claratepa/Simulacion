l <- 1.5
n <- 50 
pi <- 0.05 
pr <- 0.02 
v <- l / 30 
PV <- seq(0,1,0.1) 
datos <- data.frame()


for(pv in PV){   
  for(rep in 1:25){  
    agentes <- data.frame(x = double(), y = double(), 
                          dx = double(), dy = double(),
                          estado  = character())
    for (i in 1:n) {
      if(runif(1) < pv){ 
        e <- "R"
      } else if(runif(1) < pi){
        e <- "I"
      } else{
        e <- "S"
      }
            agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), 
                                                 y = runif(1, 0, l),
                                                 dx = runif(1, -v, v), 
                                                 dy = runif(1, -v, v),
                                                 estado = e))
            levels(agentes$estado) <- c("S", "I", "R") 
    }
      epidemia <- integer()
      r <- 0.1 
      tmax<- 100
      digitos <- floor(log(tmax, 10)) + 1
      for (tiempo in 1:tmax){ 
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      mayorinf <- max(epidemia)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
     
          a$x <- a$x + a$dx
          a$y <- a$y + a$dy
        
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
    }
  
    
    }
    
    porcinf <- (mayorinf / n) * 100
    datos <- rbind(datos, c(pv, mayorinf, porcinf))
    
  }
}
colnames(datos) <- c("Probabilidad", "Maximos", "Porcentajes")


png("pr6sim.png")
probabilidades <- datos$Probabilidad
porcentaje <- datos$Maximos
boxplot(porcentaje~probabilidades, col = rainbow(11, alpha=0.2), xlab = "Probabilidad de Vacunaci\u{F3}n", ylab = "Porcentaje M\u{E1}ximo de Infectados")
graphics.off()

aggregate(Porcentajes ~ Probabilidad, data = datos, FUN = mean)
aggregate(Porcentajes ~ Probabilidad, data = datos, FUN = sd)

anova <- aov(datos$Porcentajes ~ datos$Probabilidad)
summary(anova)
