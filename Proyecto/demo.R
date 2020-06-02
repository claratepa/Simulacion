x = runif(1)
y = runif(1)
for (age in 0:9) {
    x = c(x, runif(1))
    y = c(y, runif(1))
    tiempo = paste("Age", age)
    png(paste("demo_", age, ".png", sep=""))
    plot(x, y, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
    lines(x, y)
    graphics.off()
}
d = read.csv('demo.txt', sep=' ', header=FALSE)
names(d) = c('x', 'y')
for (age in 1:dim(d)[1]) {
    x = d$x[1:age]
    y = d$y[1:age]
    tiempo = paste("(from file) Age", age)
    png(paste("fdemo_", age, ".png", sep=""))
    plot(x, y, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))
    lines(x, y)
    graphics.off()
}
