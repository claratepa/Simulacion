library(testit)
n <- 50
G <- 6.674*10^(-11)
fg <- data.frame()
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$m <- p$m - min(p$m) + 1
p$m <- round(((n)^2) * p$m / sum(p$m))
assert(min(p$m) > 0)
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)

eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}
fuerzagravedad <- function(i){
  j <- sample(1:n, 1)
  while (j == i){
    j <- sample(1:n, 1)
  }
  mi <- p[i,]$m
  mj <- p[j,]$m
  fxi <- G*((mi*mj)/(sqrt((p[j,]$x)^2+(p[i,]$x)^2)))
  fyi <- G*((mi*mj)/(sqrt((p[j,]$y)^2+(p[i,]$y)^2)))
  return(c(i, j, fxi, fyi))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 50
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("pr9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  fg <- foreach(i = 1:n, .combine="rbind") %dopar% fuerzagravedad(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i] + fg[i, 3], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i] + fg[i, 4], 1), 0)
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("pr9_t", tl, ".png", sep=""))
  plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
  png(paste("pr9_histx_", tl, ".png", sep = ""))
  hist(p$x, main=paste("Paso", iter), freq=TRUE, col="lightyellow", xlab = "Velocidad en X")
  graphics.off()
  png(paste("pr9_histy_", tl, ".png", sep = ""))
  hist(p$y, main=paste("Paso", iter), freq=TRUE, col="aliceblue",xlab = "Velocidad en Y")
  graphics.off()
  png(paste("pr9_comp", tl, ".png", sep=""))
  pairs(p[1:4],main=paste("Paso ", tl, sep=""))
  graphics.off()

}
stopImplicitCluster()
