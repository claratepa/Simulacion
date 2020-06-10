filas = 5
columnas = 2
cuantos = 1000
png('histodemo.png', width = 600, height = 800)
par(mfrow=c(filas, columnas))
for (f in 1:filas) {
    for (c in 1:columnas) {
        titulo = sprintf('Ejemplo %d %d', f, c)
        hist(rnorm(cuantos), main=titulo, col='green')
    }
}
graphics.off()

