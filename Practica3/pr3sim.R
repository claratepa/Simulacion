datosprimos = read.csv("ordenado.txt", header=FALSE)
n = dim(datosprimos)
pocos = 500 # para probar que funcione sin que demore
vectorp = datosprimos$V1[0:pocos]
print(length(vectorp))
vectornp=vectorp*5
vectorc=c(vectorp,vectornp)

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
summary(ot)
summary(it)
summary(at)
# lee http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
print(t.test(ot, it, paired = TRUE, alternative = "two.sided"))
print(t.test(at, it, paired = TRUE, alternative = "two.sided"))
print(t.test(ot, at, paired = TRUE, alternative = "two.sided"))
