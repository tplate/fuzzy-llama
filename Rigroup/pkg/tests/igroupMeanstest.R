library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

meana = unlist(lapply(split(x,i),mean,na.rm=T))
names(meana)<-NULL
meanb <- igroupMeans(x,i,na.rm=T)
all.equal(meana, meanb)

meana = unlist(lapply(split(x,i),mean,na.rm=F))
names(meana)<-NULL
meanb <- igroupMeans(x,i,na.rm=F)
all.equal(meana, meanb)
