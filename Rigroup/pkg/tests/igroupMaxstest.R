library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

maxa = unlist(lapply(split(x,i),max,na.rm=T))
names(maxa)<-NULL
maxb <- igroupMaxs(x,i,na.rm=T)
all.equal(maxa, maxb)

maxa = unlist(lapply(split(x,i),max,na.rm=F))
names(maxa)<-NULL
maxb <- igroupMaxs(x,i,na.rm=F)
all.equal(maxa, maxb)
