library(Rigroup)

x <- rnorm(5000)
i <- rep(1:2500,2)
y <- runif(5000)
is.na(x[y > 0.8]) <- TRUE

cnta <- unlist(lapply(split(x,i),length))
names(cnta) <- NULL
cntb <- igroupCounts(x,i,na.rm=F)
all.equal(cnta,cntb)
