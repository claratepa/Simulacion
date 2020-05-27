datostiburones = read.csv("sharkdata.csv")
vectorx=datostiburones$d13C
vectory=datostiburones$d15N
plot(x=vectorx,
     y=vectory,
     main="Posición isótopica de los tiburones",
     xlab="Carbono",
     ylab="Nitrógeno",
     pch=19,
     col=datostiburones$EdadRda)

legend(x = "bottomright", legend = c("0", "1", "2", "3", "4", "5"), 
       fill = c("black", "red", "green", "blue", "aquamarine", "magenta"),
       title = "Edad")