# let's run an adoption model viz coxph

# What do we need to do here?
# Create a data set containing events by user/currency
# split spells on ... 
# every day with observed friend or self returns?

library(survival)
library(data.table)
library(ffbase)
library(reshape2)
library(survival)
library(texreg)
library(parallel)

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

# merge in currency frequency
# why?
setkey(cps,cp)
setkey(fpt,cp)
fpt <- merge(fpt,cps,all.x=TRUE)



# set up the users/currencies under observation
cp.set <- c('EURUSD','USDCZK')

uids <- aus[Npd_q %in% 2:5 & 
                med_ob_q %in% 2:5 & 
                dpnlpd_q %in% 2:5 &
                netdep_q %in% 2:5,user_id]

# reduce users under consideration
c.dt <- dt[user_id %in% uids]

# pull out all adoption events
g.min.day <- 1199145600000 / 86400000
all.adopt.es <- fpt[,list(adopt = min(openday)), by=list(user_id,cp)]

# throw out each user's first day of currency
# all.adopt.es <- all.adopt.es[order(user_id,adopt)]
all.adopt.es[, valid := adopt > min(adopt), by=user_id]
all.adopt.es <- all.adopt.es[(valid)]
all.adopt.es <- all.adopt.es[adopt > g.min.day]
all.adopt.es[,valid := NULL]

# can we pull in the whole sbc database? only need results of single day...
idx.1d <- ffwhich(ffd,gap==1)
sbc.1d <- as.data.table(as.data.frame(ffd[idx.1d,]))
print(object.size(sbc.1d), units='auto')

# create time since last trade
c.dt <- c.dt[order(user_id,day)]
c.dt[, bopen := as.integer(opened_today > 0)]
c.dt[, cumopen := cumsum(bopen) - bopen, by=user_id]
c.dt[, dlapse := 1:.N, by=list(user_id,cumopen)]
c.dt[, dlapse := as.factor(dlapse)]
c.dt[, c('imputed','bopen','cumopen') := NULL]

# we should insert user's own stats here...
sbc.1d <- sbc.1d[, c('gap','imputed','dpnl_sum','dpnl_mean','dpnl_pos','dpnl_neg') := NULL]
sbc.e1d <- sbc.1d[type == 'ego']
sbc.a1d <- sbc.1d[type == 'alter']

# make 10 day lag for ego
# make 14 day lag for alters
sbc.a14 <- rbindlist(lapply(0:13, function (x) {
        res <- data.table(sbc.a1d)
        res[, src := day]
        res[, day := day + x]
        res
    }))
sbc.e2 <- rbindlist(lapply(0:1, function (x) {
        res <- data.table(sbc.e1d)
        res[, src := day]
        res[, day := day + x]
        res
    }))
sbc.e10 <- rbindlist(lapply(2:9, function (x) {
        res <- data.table(sbc.e1d)
        res[, src := day]
        res[, day := day + x]
        res
    }))

# now collapse the fuckers
sbc.a14 <- sbc.a14[, list(ntotal.alt = sum(ntotal),
                          npos.alt = sum(npos),
                          nneg.alt = sum(nneg)
                          ), by=list(user_id, cp, day)]
sbc.e2 <- sbc.e2[, list(ntotal.e2 = sum(ntotal),
                        npos.e2 = sum(npos),
                        nneg.e2 = sum(nneg)
                        ), by=list(user_id, day)]
sbc.e10 <- sbc.e10[, list(ntotal.e10 = sum(ntotal),
                          npos.e10 = sum(npos),
                          nneg.e10 = sum(nneg)
                          ), by=list(user_id, day)]

# sort the bastard
setkey(sbc.a14, user_id, day)
setkey(sbc.e2, user_id, day)
setkey(sbc.e10, user_id, day)
setkey(c.dt,user_id, day)
c.dt <- merge(c.dt, sbc.e2, all.x=TRUE)
c.dt <- merge(c.dt, sbc.e10, all.x=TRUE)

c.dt[is.na(ntotal.e2), c('ntotal.e2', 'npos.e2', 'nneg.e2') := 0]
c.dt[is.na(ntotal.e10), c('ntotal.e10', 'npos.e10', 'nneg.e10') := 0]

cp.set <- all.adopt.es[,unique(cp)]
cp.set <- cp.set[1:4]

# what do we need in the spell split?
res <- mclapply(cp.set, mc.cores=40, mc.preschedule=FALSE,
    function (ccp) {
        print(ccp)
#        adopt.es <- fpt[cp == ccp, list(adopt=min(openday)), by=user_id]
        adopt.es <- all.adopt.es[cp == ccp]
        
        setkey(adopt.es,user_id)
        cp.dt <- merge(c.dt, adopt.es, all.x=TRUE)

        cp.dt <- cp.dt[is.na(adopt) | day <= adopt]
        cp.dt[, badopt := ifelse(day==adopt,1,0)]
        cp.dt[is.na(badopt), badopt := 0]

        cp.dt[, cp := ccp]
        cp.dt[, adopt := NULL]

        setkey(cp.dt, user_id, day)
        cp.dt <- merge(cp.dt, sbc.a14[cp == ccp, !'cp', with=FALSE], all.x=TRUE) 
        cp.dt[is.na(ntotal.alt), c('ntotal.alt', 'npos.alt', 'nneg.alt') := 0]
        # 
        cp.dt
    })











stop()
stopifnot(FALSE)

# assign user group
aus <- aus[user_id %in% uids]
aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,netdep_q,Npd_q,dpnlpd_q)]
# aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,Npd_q,dpnlpd_q)]
aus[,ugrpN := .N, by=ugrp]
summary(aus[,.N,by=ugrp])
quantile(aus[,.N,by=ugrp][,N], seq(0,1,0.1))

# drop single user groups
# uids <- aus[ugrpN > 1, user_id]
uids <- aus[,user_id]

# pull out user-restricted observations
c.dt <- dt[user_id %in% uids]
c.fpt <- fpt[user_id %in% uids]

# preprocess c.dt stuff
c.dt[,bopen:=as.integer(opened_today>0)]

# merge in user statistics
setkey(c.dt,user_id)
setkey(aus,user_id)

c.dt <- merge(c.dt,aus)
