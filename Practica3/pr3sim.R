datosprimos = read.csv("ordenado.txt", header=FALSE)
vectorp = datosprimos$V1
vectornp=vectorp*5
vectorc=c(vectorp,vectornp)

t <- proc.time()

primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
original <- vectorc
invertido <- rev(vectorc)
replicas <- 10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) 
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) 
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) 
}
stopImplicitCluster()
proc.time() - t
summary(ot)
summary(it)
summary(at)

