datostiburones = read.csv("shark1.csv")
vectorx=datostiburones$d13C
vectory=datostiburones$d15N



x = vectorx
y = vectory
for (age in 1:4) {
    x = c(x, vectorx)
    y = c(y, vectory)
    tiempo = paste("Age", age)
    png(paste("demo_", age, ".png", sep=""))
    plot(x, y, xlim = c(-18, -14), ylim = c(7, 14))
    lines(x, y)
    graphics.off()
}

