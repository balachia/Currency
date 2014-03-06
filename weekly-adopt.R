library(ffbase)
library(data.table)
library(lubridate)
library(parallel)
library(stringr)
library(survival)
library(texreg)



# MATCH FILE SETTINGS TO HOST MACHINE
MC.CORES <- 2
if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/Currensee/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
    
    # yen match
    yenre <- str_match(Sys.info()[['nodename']], 'yen(\\d)\\.stanford\\.edu')
    if (yenre[1,2] == '5') {
        MC.CORES <- 60
    } else if (yenre[1,2] == '6' | yenre[1,2] == '7') {
        MC.CORES <- 24
    }
} else {
    DATA.DIR <- '~/Data/Currensee/'
    OUT.DIR <- '~/Dropbox/Currensee Project/writing/'
    CODE.DIR <- '~/Dropbox/Currensee Project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)

# LOAD IN DATA
aus <- readRDS('./Rds/active-user-quantiles.Rds')
dt <- readRDS('./Rds/day.stats-0-1samp.Rds')
fpt <- readRDS('./Rds/forexposition.Rds')
ffdfns <- load.ffdf('./ffdb/sbc')
ffd <- ffdfns$ffd



# PRELIMINARY MUCKING AROUND
# put days in fpt stuff
fpt[,openday := floor((opendate / 86400000.0) + 0.125)]
fpt[,cp := paste0(currency1,currency2)]

# get currency use frequency
cps <- fpt[, list(cpN=.N), by=cp]
cps <- cps[order(-cps$cpN)]
cps[,rank:=.I]



# restrict to users in middle quantiles
uids <- aus[Npd_q %in% 2:5 & 
                med_ob_q %in% 2:5 & 
                dpnlpd_q %in% 2:5 &
                netdep_q %in% 2:5,user_id]

# MAKE USER GROUPS
aus <- aus[user_id %in% uids]
aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,netdep_q,Npd_q,dpnlpd_q)]
# aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,Npd_q,dpnlpd_q)]
aus[,ugrpN := .N, by=ugrp]
summary(aus[,.N,by=ugrp])
quantile(aus[,.N,by=ugrp][,N], seq(0,1,0.1))

aus.bad.cols <- names(aus)[grep('_q$', names(aus))]

# throw out single user groups
# uids <- aus[ugrpN > 1, user_id]

# reduce users under consideration
c.dt <- dt[user_id %in% uids]

# ADD IN USER STATISTICS
setkey(aus, user_id)
setkey(c.dt, user_id)
c.dt <- merge(c.dt, aus[, !aus.bad.cols, with=FALSE], all.x=TRUE)



# MAKE ADOPTION EVENTS
# pull out all adoption events
g.min.day <- 1199145600000 / 86400000
all.adopt.es <- fpt[,list(adopt = min(openday)), by=list(user_id,cp)]

# throw out each user's first day of currency
# all.adopt.es <- all.adopt.es[order(user_id,adopt)]
all.adopt.es[, valid := adopt > min(adopt), by=user_id]
all.adopt.es <- all.adopt.es[(valid)]
all.adopt.es <- all.adopt.es[adopt > g.min.day]
all.adopt.es[,valid := NULL]



# GET RECENT RESULTS BY ACTOR TYPE
# can we pull in the whole sbc database? only need results of single day...
idx.1d <- ffwhich(ffd,gap==1)
sbc.1d <- as.data.table(as.data.frame(ffd[idx.1d,]))
print(object.size(sbc.1d), units='auto')

# split out currency success between ego and alter
sbc.1d <- sbc.1d[, c('gap','imputed','dpnl_sum','dpnl_mean','dpnl_pos','dpnl_neg') := NULL]
sbc.e1d <- sbc.1d[type == 'ego']
sbc.a1d <- sbc.1d[type == 'alter']

sbc.a14 <- build.lag(sbc.a1d, 0:13, c('user_id','cp','day'), '.a14')
sbc.e2 <- build.lag(sbc.e1d, 0:1, c('user_id','day'), '.e2')
sbc.e10 <- build.lag(sbc.e1d, 2:9, c('user_id','day'),'.e10')

# MAKE TRADE EVENTS, INTER-TRADE TIMES...
# create time since last trade
c.dt <- c.dt[order(user_id,day)]
c.dt[, bopen := as.integer(opened_today > 0)]
c.dt[, cumopen := cumsum(bopen) - bopen, by=user_id]
c.dt[, dlapse := 1:.N, by=list(user_id,cumopen)]
c.dt[, c('imputed','bopen','cumopen') := NULL]

# MERGE IN EGO RECENT BEHAVIOR
setkey(sbc.a14, user_id, day)
setkey(sbc.e2, user_id, day)
setkey(sbc.e10, user_id, day)
setkey(c.dt,user_id, day)
c.dt <- merge(c.dt, sbc.e2, all.x=TRUE)
c.dt <- merge(c.dt, sbc.e10, all.x=TRUE)

c.dt[is.na(ntotal.e2), c('ntotal.e2', 'npos.e2', 'nneg.e2') := 0]
c.dt[is.na(ntotal.e10), c('ntotal.e10', 'npos.e10', 'nneg.e10') := 0]

c.dt[,daysactive := day - minday + 1]

# BUILD THE WHOLE BASTARD
# sample out some currencies
cp.set <- cps[rank %% 6 == 1, cp]

ptm <- proc.time()
res <- mclapply(cp.set, mc.cores=MC.CORES, mc.preschedule=FALSE,
    function (ccp) {
        print(ccp)

        # pull out currency-only adoption events
        adopt.es <- all.adopt.es[cp == ccp]

        # get currency pair rank
        cp.rank <- cps[cp==ccp, rank]
        
        # merge in adoption events
        setkey(adopt.es,user_id)
        cp.dt <- merge(c.dt, adopt.es, all.x=TRUE)

        # drop observations after first adoption
        cp.dt <- cp.dt[is.na(adopt) | day <= adopt]
        cp.dt[, badopt := ifelse(day==adopt,1,0)]
        cp.dt[is.na(badopt), badopt := 0]

        # merge in currency statistics
        cp.dt[, cp := ccp]
        cp.dt[,rank := cp.rank]
        cp.dt[, adopt := NULL]

        # merge in alter behavior
        setkey(cp.dt, user_id, day)
        cp.dt <- merge(cp.dt, sbc.a14[cp == ccp, !'cp', with=FALSE], all.x=TRUE) 
        cp.dt[is.na(ntotal.a14), c('ntotal.a14', 'npos.a14', 'nneg.a14') := 0]

        cp.dt
    })
print(proc.time() - ptm)

print(system.time(all.adopts <- rbindlist(res)))
print(dim(all.adopts))
print(object.size(all.adopts), units='auto')

# now, let's try to throw down a matching...

q.names <- c('totaldpnl','openbalance')
q.names <- c('totaldpnl','posdpnl')
q.names <- c('totaldpnl','posdpnl','openbalance')

q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10')
#q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','npos.e10')
q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','daysactive')
#q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','daysactive','rank')

s.names <- c('ugrp')
#s.names <- c('ugrp','cp')

grps <- poor.cem(all.adopts,
                 keys=c('user_id','day','cp'), 
                 snames=s.names,
                 qnames=q.names,
                 bkeys=c('badopt'))

#grps[,evtypes := length(unique(badopt)), by=grp]
#print(grps[evtypes == 1, sum(badopt)])

setkey(all.adopts,cp,user_id,day)
setkey(grps,cp,user_id,day)
all.adopts2 <- merge(all.adopts,grps,all=TRUE)

all.adopts2[, hasboth := length(unique(badopt)) - 1, by=grp]
print(all.adopts2[hasboth == 0, sum(badopt)])

# diagnostics...
unique.users <- all.adopts2[hasboth == 1, length(unique(user_id)), by=grp]

# prune out non-informative groups
all.adopts2 <- all.adopts2[hasboth == 1]



# ANALYSIS TIME???
m1 <- clogit(badopt ~ ntotal.a14 + strata(grp), data = all.adopts2)
print(summary(m1))
m2 <- clogit(badopt ~ ntotal.a14 + npos.a14 + strata(grp), data = all.adopts2)
print(summary(m2))
m3 <- clogit(badopt ~ ntotal.a14*npos.a14 + strata(grp), data = all.adopts2)
print(summary(m3))

me1 <- clogit(badopt ~ ntotal.a14*npos.e2 + npos.a14*npos.e2 + strata(grp), data = all.adopts2)
print(summary(me1))
me2 <- clogit(badopt ~ ntotal.a14*npos.a14*npos.e2 + strata(grp), data = all.adopts2)
print(summary(me2))

mr1 <- clogit(badopt ~ ntotal.a14*rank + npos.a14*rank + strata(grp), data = all.adopts2)
print(summary(mr1))
mr2 <- clogit(badopt ~ ntotal.a14*npos.a14*rank + strata(grp), data = all.adopts2)
print(summary(mr1))

# OUTPUT TIME
setwd(paste0(OUT.DIR,'tables/'))

#texreg(list(md1,md2,md3,md4),
#       file='matching-dlrs.tex', label='tab:dlrs', digits=3, float.pos='htb',
#       caption='Conditional Logit: Dollar Returns in Past 2 Days',
#       custom.coef.names=c('Mean \\$ Returns','Total \\$ Returns','Mean:Total'))

texreg(list(m1,m2,m3),
       file='adopts-base.tex', label='tab:adopt-base', digits=3, float.pos='htb',
       caption='Conditional Logit: Baseline Models of Currency Pair Adoption',
       custom.coef.names=NULL)

texreg(list(me1,me2),
       file='adopts-egoret.tex', label='tab:adopt-ego', digits=3, float.pos='htb',
       caption='Conditional Logit: Currency Pair Adoption with Ego Returns',
       custom.coef.names=NULL)

texreg(list(mr1,mr2),
       file='adopts-rank.tex', label='tab:adopt-rank', digits=3, float.pos='htb',
       caption='Conditional Logit: Currency Pair Adoption with Rank Interaction',
       custom.coef.names=NULL)

