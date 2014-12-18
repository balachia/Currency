# what the hell is the point of this file?

library(ffbase)
library(data.table)
library(parallel)

rm(list=ls())

setwd('/archive/gsb/vashevko/Currensee/')

aus <- readRDS('~/2YP/data/active-user-quantiles.Rds')
aus <- aus[,list(user_id,dayson)]
setkey(aus,user_id)

ffdfns <- load.ffdf('./ffdb/sbc/')
ffd <- ffdfns$ffd

open(ffd)

cps <- unique(ffd$cp)

#cps <- cps[1:1]
cps <- cps[1:length(cps)]
par.cores <- 64

res <- mclapply(cps, mc.cores=par.cores, mc.preschedule=FALSE, FUN=function(ccp) {
    #print('test')
    idx <- ffwhich(ffd, cp==ccp & ((gap == 1 & type=='ego') | (gap==29 & type=='alter')))
    #print('bork1')
    
    c.dt <- as.data.table(as.data.frame(ffd[idx,]))
    #print('bork2')
    
    c.dt[,day := day - 1]
    print(c.dt)
    
    c.ego <- c.dt[type=='ego',list(user_id,day,bego=1)]
    c.alt <- c.dt[type=='alter',list(user_id,day,balt=1)]
    setkey(c.ego, user_id, day)
    setkey(c.alt, user_id, day)

    c.all <- merge(c.ego, c.alt, all=TRUE)
    
    print(c.all)

    c.res <- c.all[,list(nboth = .SD[bego==balt, .N],
                         njego = .SD[bego==1 & is.na(balt), .N],
                         njalt = .SD[balt==1 & is.na(bego), .N]
                         ),by=list(user_id)]
    setkey(c.res,user_id)
    c.res <- merge(c.res,aus, all=TRUE)
    c.res[,cp := ccp]

    print(c.res)
    print(summary(c.res))
    
    c.res
  })

res.dt <- rbindlist(res)
saveRDS(res.dt, 'currency-correlations.Rds')
