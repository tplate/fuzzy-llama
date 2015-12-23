library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

anya <- unlist(lapply(split((x>1.0),i),any,na.rm=T))
names(anya)<-NULL
anyb <- igroupAnys((x>1.0),i,na.rm=T) 
all.equal(anya,anyb)

anya <- unlist(lapply(split((x>1.0),i),any,na.rm=F))
names(anya)<-NULL
anyb <- igroupAnys((x>1.0),i,na.rm=F)
all.equal(anya,anyb)

