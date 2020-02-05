registro = data.frame()
dimension = 8
potencia = 10
for (pot in 5:potencia){
  duracion <- 2^potencia
  for (dim in 1:dimension) {
    
    datos = numeric()
    repeticiones = 50
    for (replica in 1:repeticiones) {
      resultado = FALSE
      pos <- rep(0, dim)
      for (t in 1:duracion) {
        modificar = sample(1:dim, 1)
        if (runif(1) < 0.5) {
          pos[modificar] =  pos[modificar] + 1
        } else {
          pos[modificar] =  pos[modificar] - 1
        }
        if (all(pos == 0)) {
          resultado = TRUE
          break
        }
      }
      datos = c(datos, resultado)
}
  porc = 100 * sum(datos) / repeticiones
  registro = rbind(registro, c(pot, porc, dim))
  }
}
names(registro) = c("pot", "porc", "dim")
sink('registro.txt')
print(registro)
sink()
png("pr1sim.png", width=800, height=800, units='px')
par(cex.lab=2) 
par(cex.axis=2) 
boxplot(porc ~ dim, 
data =  registro,
xlab="Dimensi\u{F3}n",
ylab="Porcentaje de regreso al punto de origen",
col=rainbow(8, alpha=0.2),
border = rainbow(8, v=0.6)
)
dev.off()

