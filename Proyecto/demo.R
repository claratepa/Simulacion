x = runif(1)
y = runif(1)
for (age in 0:9) {
    x = c(x, runif(1))
    y = c(y, runif(1))
    tiempo = paste("Age", age)
    png(paste("demo_", age, ".png", sep=""))
    plot(x, y, xlim = c(0, 1))
    lines(x, y)
    graphics.off()
}

