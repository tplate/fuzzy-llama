library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

proda = unlist(lapply(split(x,i),prod,na.rm=T))
names(proda)<-NULL
prodb <- igroupProds(x,i,na.rm=T)
all.equal(proda, prodb)

proda = unlist(lapply(split(x,i),prod,na.rm=F))
names(proda)<-NULL
prodb <- igroupProds(x,i,na.rm=F)
all.equal(proda, prodb)
