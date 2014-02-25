# let's run an adoption model viz coxph

library(survival)
library(data.table)
library(ffbase)
library(reshape2)
library(survival)
library(texreg)

rm(list=ls())

qcuts <- function(x, qs) {
    cut(x, unique(quantile(x, qs, na.rm=TRUE)), labels=FALSE, include.lowest=TRUE)
}

poor.cem <- function(dt, keys, snames=NULL, qnames=NULL, bkeys=keys) {
    kdt <- dt[,c(keys,snames),with=FALSE]
    setkeyv(dt,keys)
    setkeyv(kdt,keys)
    
    print(kdt)
#     print(names(dt))
    
    cat('making quantiles\n')
    lapply(qnames, function (x) {
        cat('processing ',x,'\n')
        kdt[,paste0(x,'_q') := qcuts(dt[,get(x)], seq(0,1,0.1))]
        NULL
    })
    
    allnames <- c(snames, paste0(qnames,'_q'))
    
    cat('making groups\n')
    kdt[,grp := .GRP, by=allnames]
    
    ngrps <- kdt[,max(grp)]
    cat('# Groups:', ngrps, '\n')

    cat('balance statistics\n')
    cat('# observations\n')
    dat <- kdt[,.N,by=grp][,N]
    print(summary(dat))
    print(quantile(dat,seq(0,1,0.1), na.rm=TRUE))
    cat('\n')
    
    for(key in bkeys) {
        cat(key,':\n')
#        dat <- kdt[,dim(.SD[,.N,by=key])[1],by=grp][,V1]
        dat <- kdt[,length(unique(get(key))),by=grp][,V1]

        # try a parallel version...
#        dats <- mclapply(split(1:ngrps, factor(1:ngrps %% (64 * 2))), 
#                         mc.cores=64, mc.preschedule=FALSE,
#                         function (subgrps) {
#                             sub.dat <- kdt[grp %in% subgrps, length(unique(key)), by=grp][,V1]
#                             sub.dat
#                         })
#        dat <- rbindlist(dats)
#        print(dats)
#        print(dat)

        print(summary(dat))
        print(quantile(dat,seq(0,1,0.1), na.rm=TRUE))
        cat('# == 1 ::', sum(dat==1), '(', sum(dat==1) / length(dat), '%)\n')
        cat('\n')
    }
    
    kdt[,c(keys,'grp'), with=FALSE]
}

if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/Currensee/'
    OUT.DIR <- '~/2YP/writing/'
} else {
    DATA.DIR <- '~/Data/Currensee/'
    OUT.DIR <- '~/Dropbox/Currensee Project/writing/'
}

setwd(DATA.DIR)

# load in data
aus <- readRDS('./Rds/active-user-quantiles.Rds')
dt <- readRDS('./Rds/day.stats-0-1samp.Rds')
fpt <- readRDS('./Rds/forexposition.Rds')
ffdfns <- load.ffdf('./ffdb/sbc')
ffd <- ffdfns$ffd



# put days in fpt stuff
fpt[,openday := floor((opendate / 86400000.0) + 0.125)]
fpt[,cp := paste0(currency1,currency2)]

# get currency use frequency
cps <- fpt[, list(cpN=.N), by=cp]
cps <- cps[order(-cps$cpN)]
cps[,rank:=.I]
cps[,major5 := as.numeric(rank <= 5)]
cps[,major10 := as.numeric(rank <= 10)]
cps[,major15 := as.numeric(rank <= 15)]
cps[,major20 := as.numeric(rank <= 20)]

# merge in currency frequency
setkey(cps,cp)
setkey(fpt,cp)
fpt <- merge(fpt,cps,all.x=TRUE)
