filas = 5
columnas = 2
cuantos = 1000
png('histodemo.png', width = 600, height = 800)
par(mfrow=c(filas, columnas))
for (f in 1:filas) {
    media = 2 * f
    for (c in 1:columnas) {
        desv = c / 2
        titulo = sprintf('Ejemplo %d %d', f, c)
        hist(rnorm(cuantos, media, desv),
             main = titulo,
             col = 'green',
             xlim = c(-5, 15),
             ylim = c(0, 400))
    }
}
graphics.off()

