library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

mina = unlist(lapply(split(x,i),min,na.rm=T))
names(mina)<-NULL
minb <- igroupMins(x,i,na.rm=T)
all.equal(mina, minb)

mina = unlist(lapply(split(x,i),min,na.rm=F))
names(mina)<-NULL
minb <- igroupMins(x,i,na.rm=F)
all.equal(mina, minb)
