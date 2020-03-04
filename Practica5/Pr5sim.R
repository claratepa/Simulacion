desde <- 3
hasta <- 7
pedazo <- 50000
cuantos <- 500
info <- data.frame()
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
suppressMessages(library(distr))
generador  <- r(AbscontDistribution(d = g)) # creamos un generador            
parte <- function() {
    valores <- generador(muestra)
    return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

for (muestra in c(10, 100, 1000, 10000, 100000)){
	for (repeticiones in 1:40) {
		montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
		stopImplicitCluster()
		integral <- sum(montecarlo) / (cuantos * muestra)
		resultado <- ((pi / 2) * integral)
		comparacion<-(0.048834-resultado)
		info <- rbind(info, c(repeticiones, muestra, resultado, comparacion))
	}
}
colnames(info) <- c("Repeticiones", "Muestra", "Resultado", "Comparacion")
info

color <- c("light blue", "light green", "light pink", "light yellow", "light grey")
png("ErrorPr5sim.png") 
boxplot(data=info,Comparacion~Muestra,xlab="Muestra",ylab="Error", col=color)
abline(h=0, col="red")
graphics.off()


png("ComparacionPr5sim.png") 
boxplot(data=info,Resultado~Muestra,xlab="Muestra",ylab="Aproximaci\u00F3n", col=color)
abline(h=0.048834, col="red")
graphics.off()
