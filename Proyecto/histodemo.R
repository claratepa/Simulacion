filas = 5
columnas = 4
cuantos = 1000
png('histodemo.png', width = 600, height = 800)
par(mfrow=c(filas, columnas))
for (f in 1:filas) {
    media = 2 * f
    for (c in 1:columnas) {
        desv = sqrt(c)
        titulo = sprintf('Ejemplo %d %d', f, c)
        hist(rnorm(cuantos, media, desv),
             main = titulo,
             col = 'green',
             xlim = c(-5, 20),
             ylim = c(0, 300))
    }
}
graphics.off()

