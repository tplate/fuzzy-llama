library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

suma = unlist(lapply(split(x,i),sum,na.rm=T))
names(suma) <- NULL
sumb = igroupSums(x,i,na.rm=T)
all.equal(suma,sumb)

suma = unlist(lapply(split(x,i),sum,na.rm=F))
names(suma) <- NULL
sumb = igroupSums(x,i,na.rm=F)
all.equal(suma,sumb)
