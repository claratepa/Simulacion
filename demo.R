registro = data.frame()
dimension = 8
for (dim in 1:dimension){
  for (potencia in 5:10) {
    duracion <- 2^potencia
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
}
  porc = 100 * sum(datos) / repeticiones
  registro = rbind(registro, c(potencia, porc, dim))
}
names(registro) = c("pot", "porc", "dim")
boxplot(formula = porc ~ dim, data =  registro)

