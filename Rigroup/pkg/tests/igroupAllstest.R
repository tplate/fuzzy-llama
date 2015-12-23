library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

alla <- unlist(lapply(split((x>1.0),i),all,na.rm=T))
names(alla)<-NULL
allb <- igroupAlls((x>1.0),i,na.rm=T)
all.equal(alla,allb)

alla <- unlist(lapply(split((x>1.0),i),all,na.rm=F))
names(alla)<-NULL
allb <- igroupAlls((x>1.0),i,na.rm=F)
all.equal(alla,allb)
