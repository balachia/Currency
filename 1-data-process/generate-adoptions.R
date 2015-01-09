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
    DATA.DIR <- '/archive/gsb/vashevko/forex/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
    
    # yen match
    yenre <- str_match(Sys.info()[['nodename']], 'yen(\\d)\\.stanford\\.edu')
    if (yenre[1,2] == '5') {
        MC.CORES <- 60
    } else if (yenre[1,2] == '6' | yenre[1,2] == '7') {
        MC.CORES <- 12
#        MC.CORES <- 6
    }
} else {
    DATA.DIR <- '~/Data/forex/'
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
uids <- aus[, user_id]
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
lagged.opens <- fpt[,list(openday),by=list(user_id,cp)]
lagged.opens <- lagged.opens[order(openday)]
lagged.opens <- lagged.opens[openday > g.min.day]
lagged.opens[,l.openday := c(-Inf,openday[1:.N-1]), by=list(user_id,cp)]
lagged.opens[,lapse := openday - l.openday]
lagged.opens <- lagged.opens[lapse > 0]

# toss out lagged adoptions below 10 days?
adopt.stats <- lagged.opens[,list(.N,
                   min=min(lapse),
                   max=max(lapse),
                   p10=quantile(lapse,0.1),
                   p20=quantile(lapse,0.2),
                   p30=quantile(lapse,0.3),
                   p40=quantile(lapse,0.4),
                   p50=quantile(lapse,0.5),
                   p60=quantile(lapse,0.6),
                   p70=quantile(lapse,0.7),
                   p80=quantile(lapse,0.8),
                   p90=quantile(lapse,0.9)
                   ),by=cp]
adopt.stats <- adopt.stats[order(-N)]

flex.adopts <- lagged.opens[lapse > 0]
flex.adopts[,l.openday := NULL]
setnames(flex.adopts,c('openday','lapse'),c('day','adlapse'))


# throw out each user's first day of currency
# all.adopt.es <- all.adopt.es[order(user_id,adopt)]
all.adopt.es[, valid := adopt > min(adopt), by=user_id]
all.adopt.es <- all.adopt.es[(valid)]
all.adopt.es <- all.adopt.es[adopt > g.min.day]
all.adopt.es[,valid := NULL]

# how do i do this in the flex adopt mode....
flex.adopts[, first.curr := day == min(day), by=user_id]
flex.adopts[, first.curr := any(first.curr), by=list(user_id,cp)]
flex.adopts <- flex.adopts[(!first.curr)]
flex.adopts[, first.curr := NULL]



# USER INACTIVITY
# how should we look at periods of inactivity?
inactivity <- fpt[order(openday), list(openday), by=user_id]
inactivity <- inactivity[openday > g.min.day]
inactivity[, L.openday := c(tail(openday,-1),Inf), by=user_id]
inactivity[, lapse := L.openday - openday]
inactivity <- inactivity[lapse > 0]
inact.by.user <- inactivity[, list(p75 = quantile(lapse,0.75),
                                   p90 = quantile(lapse,0.90),
                                   p95 = quantile(lapse,0.95),
                                   p96 = quantile(lapse,0.96),
                                   p97 = quantile(lapse,0.97),
                                   p98 = quantile(lapse,0.98),
                                   p99 = quantile(lapse,0.99)
                                   ), by=user_id]
print(summary(inact.by.user))



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
sbc.e60 <- build.lag(sbc.e1d, 0:59, c('user_id','day'),'.e60')

# only need e60 to check for user activity
sbc.e60[, c('ntotal.e60','npos.e60','nneg.e60', 'nfr.e60') := NULL]
sbc.e60[, user.active := 1]

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
setkey(sbc.e60, user_id, day)
setkey(c.dt,user_id, day)
c.dt <- merge(c.dt, sbc.e2, all.x=TRUE)
c.dt <- merge(c.dt, sbc.e10, all.x=TRUE)
c.dt <- merge(c.dt, sbc.e60, all.x=TRUE)

c.dt[is.na(ntotal.e2), c('ntotal.e2', 'npos.e2', 'nneg.e2', 'nfr.e2') := 0]
c.dt[is.na(ntotal.e10), c('ntotal.e10', 'npos.e10', 'nneg.e10', 'nfr.e10') := 0]
c.dt[is.na(user.active), c('user.active') := 0]

c.dt[,daysactive := day - minday + 1]

# BENCHMARKING
# by community, self overall, self recent
c.dt[, all.winfrac := sbc.e1d[,sum(npos) / sum(ntotal)]]
user.bench <- sbc.e1d[,list(user.winfrac = sum(npos) / sum(ntotal)), by=user_id]
setkey(user.bench, user_id)
c.dt <- merge(c.dt, user.bench, by='user_id', all.x=TRUE)

# impute 1:1 ratio to unobserved users (???)
c.dt[is.na(user.winfrac), user.winfrac := 0.5]

# add in recent shit based on past 8 days...
c.dt[,winfrac.e10 := npos.e10 / ntotal.e10]
c.dt[is.na(winfrac.e10), winfrac.e10 := 0.5]



# BUILD THE WHOLE BASTARD
# sample out some currencies
cp.set <- cps[rank %% 6 == 1, cp]
cp.set <- cps[, cp]
#cp.set <- cps[rank == 1, cp]
setkey(c.dt,user_id,day)

ptm <- proc.time()
res <- mclapply(cp.set, mc.cores=MC.CORES, mc.preschedule=FALSE,
    function (ccp) {
#        print(ccp)
        cat(ccp, '::', cps[cp==ccp, rank], '\n')

        # pull out currency-only adoption events
#        adopt.es <- all.adopt.es[cp == ccp]
        adopt.es <- flex.adopts[cp == ccp]

        # get currency pair rank
        cp.rank <- cps[cp==ccp, rank]
        
        # merge in adoption events
        setkey(adopt.es,user_id,day)
        cp.dt <- merge(c.dt, adopt.es, all.x=TRUE)
        cp.dt <- cp.dt[order(user_id,day)]

        print(ccp)
        print(object.size(cp.dt),units='auto')
#        print(gc())

        # drop observations after first adoption
        # fuck that

        # instead mark days since last adoption...
        cp.dt[, badopt2 := as.numeric(!is.na(adlapse))]
        cp.dt[, cum.adopt := cumsum(badopt2) - badopt2, by=user_id]
        cp.dt[cum.adopt == 0, uselapse := Inf]
        cp.dt[cum.adopt > 0, uselapse := as.numeric(1:.N), by=list(user_id,cum.adopt)]
        cp.dt[, badopt := as.numeric(is.infinite(adlapse))]

#        cp.dt <- cp.dt[is.na(adopt) | day <= adopt]
#        cp.dt[, badopt := ifelse(day==adopt & is.infinite(adlapse),1,0)]
#        cp.dt[, badopt2 := ifelse(day==adopt & is.infinite(adlapse),1,0)]
#        cp.dt[is.na(badopt), badopt := 0]

        # merge in currency statistics
        cp.dt[, cp := ccp]
        cp.dt[,rank := cp.rank]
        cp.dt[, adopt := NULL]

        # merge in alter behavior
        setkey(cp.dt, user_id, day)
        cp.dt <- merge(cp.dt, sbc.a14[cp == ccp, !'cp', with=FALSE], all.x=TRUE) 
        cp.dt[is.na(ntotal.a14), c('ntotal.a14', 'npos.a14', 'nneg.a14', 'nfr.a14') := 0]

        # instead of storing in memory, let's write out all this out as ffdf
        # need to recode characters to factors
        for(x in names(cp.dt))
        {
            if (class(cp.dt[,get(x)]) == 'character')
                cp.dt[, x := as.factor(get(x)), with=FALSE]
        }
#        print(cp.dt)
#        lapply(cp.dt, function(x) print(class(x)))

        cp.ffd <- as.ffdf(cp.dt)
        unlink(paste0('./ffdb/cp-adopts/',ccp,'/'), recursive=TRUE)
        save.ffdf(cp.ffd, dir=paste0('./ffdb/cp-adopts/',ccp,'/'), overwrite=TRUE)
        rm(cp.dt)
        close(cp.ffd)

        gc()

#        cp.dt
        0
    })
print(proc.time() - ptm)

unlink('./ffdb/all-adopts/', recursive=TRUE)
ad.ffd <- NULL
for (ccp in cp.set) {
    print(ccp)
    ad.ffd0 <- load.ffdf(paste0('./ffdb/cp-adopts/',ccp,'/'))$cp.ffd
    if (is.null(ad.ffd)) {
        ad.ffd <- clone.ffdf(ad.ffd0)
    } else {
        ad.ffd <- ffdfappend(ad.ffd, ad.ffd0)
    }
    save.ffdf(ad.ffd, dir='./ffdb/all-adopts/', overwrite=TRUE)
    close(ad.ffd0)
}

ad.ffd$id <- ff(1:nrow(ad.ffd))

save.ffdf(ad.ffd, dir='./ffdb/all-adopts/', overwrite=TRUE)
close(ad.ffd)
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

gc()



# MATCHING
# now, let's try to throw down a matching...

q.names <- c('totaldpnl','openbalance')
q.names <- c('totaldpnl','posdpnl')
q.names <- c('totaldpnl','posdpnl','openbalance')

q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10')
#q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','npos.e10')
q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','daysactive')
#q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','daysactive','rank')

s.names <- c('ugrp')
s.names <- c('user_id')
#s.names <- c('ugrp','cp')

all.adopts <- as.data.table(as.data.frame(
    ad.ffd[unique(c('id','badopt', q.names, s.names))]
                                          ))

ptm <- proc.time()
#q.names <- c('totaldpnl','posdpnl','openbalance','ntotal.e10','daysactive','rank')

s.names <- c('ugrp')
s.names <- c('user_id')
#s.names <- c('ugrp','cp')

all.adopts <- as.data.table(as.data.frame(
    ad.ffd[unique(c('id','badopt', q.names, s.names))]
                                          ))

ptm <- proc.time()
#grps <- poor.cem(all.adopts,
#                 keys=c('user_id','day','cp'), 
#                 snames=s.names,
#                 qnames=q.names,
#                 bkeys=c('badopt'))
grps <- poor.cem(all.adopts,
                 keys=c('id'),
                 snames=s.names,
                 qnames=q.names,
                 bkeys=c('badopt'))
print(proc.time() - ptm)

gc()

#grps[,evtypes := length(unique(badopt)), by=grp]
#print(grps[evtypes == 1, sum(badopt)])



# MATCHING POST-PROCESSING
# finally, add in groups and shit
grp.ffd <- as.ffdf(grps)
idx.x <- ffdforder(ad.ffd['id'])
idx.y <- ffdforder(grp.ffd['id'])

ad.ffd$grp <- ff(0, nrow(ad.ffd), vmode='integer')
ad.ffd$grpN <- ff(0, nrow(ad.ffd), vmode='integer')

#ad.ffd$grp[idx.x] <- grp.ffd$grp[idx.y]
ad.ffd[idx.x, 'grp'] <- grp.ffd[idx.y, 'grp']
ad.ffd[idx.x, 'grpN'] <- grp.ffd[idx.y, 'grpN']

save.ffdf(ad.ffd, dir='./ffdb/all-adopts/', overwrite=TRUE)
close(ad.ffd)
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

rm(grps)
gc()

#setkey(all.adopts,cp,user_id,day)
#setkey(grps,cp,user_id,day)
#all.adopts2 <- merge(all.adopts,grps,all=TRUE)
#all.adopts.full <- merge(all.adopts,grps,all=TRUE)

#rm(all.adopts)
#gc()

#all.adopts2[, hasboth := length(unique(badopt)) - 1, by=grp]
#print(all.adopts2[hasboth == 0, sum(badopt)])

hasboth <- ffdfdply(x=ad.ffd[ff(1:10000),], split=factor(ad.ffd[ff(1:10000),]$grp),
                    FUN=function(x) {
                        test <- 0 %in% x$badopt2 & 1 %in% x$badopt2
                        print(class(x))
                        test <- data.frame(hasboth2=test)
                        print(test)
                        test
                    }, trace=TRUE)
hasboth <- ffdfdply(x=ad.ffd, split=ad.ffd$grp,
                    FUN=function(x) {ff(0) %in% x$badopt2 & ff(1) %in% x$badopt2}, trace=TRUE)

all.adopts.full[, hasboth := length(unique(badopt)) - 1, by=grp]
all.adopts.full[, hasboth2 := length(unique(badopt2)) - 1, by=grp]
print(all.adopts.full[hasboth == 0, sum(badopt)])
print(all.adopts.full[hasboth2 == 0, sum(badopt2)])


# diagnostics...
#unique.users <- all.adopts2[hasboth == 1, length(unique(user_id)), by=grp]
unique.users <- all.adopts.full[hasboth2 == 1, length(unique(user_id)), by=grp]

# prune out non-informative groups
#all.adopts2 <- all.adopts2[hasboth == 1]
all.adopts.full <- all.adopts.full[hasboth2 == 1]
gc()



# ADDITIONAL IMPORTANT METRICS
# make coarse rank
all.adopts.full[, c('oddball5','oddball10','oddball15','oddball20') := 
            list(as.numeric(rank > 5),
                 as.numeric(rank > 10),
                 as.numeric(rank > 15),
                 as.numeric(rank > 20))]

# have to consider alter 0 trade days separately...
all.adopts.full[, ntaltGT0 := as.numeric(ntotal.a14 > 0)]

# trichotomize alter, self results
all.adopts.full[, ntri.a14 := as.factor(sign(npos.a14 - nneg.a14))]
all.adopts.full[ntri.a14 == 0, ntri.a14 := as.factor(as.numeric(ntotal.a14>0))]

# trichotomize with benchmark
all.adopts.full[, winfrac.e2 := npos.e2 / ntotal.e2]
all.adopts.full[, ntri.e2 := as.factor(sign(winfrac.e2 - 0.5))]
all.adopts.full[, ntri.bc.e2 := as.factor(sign(winfrac.e2 - all.winfrac))]
all.adopts.full[, ntri.be.e2 := as.factor(sign(winfrac.e2 - user.winfrac))]
all.adopts.full[, ntri.b10.e2 := as.factor(sign(winfrac.e2 - winfrac.e10))]
all.adopts.full[ntotal.e2 == 0,
                c('ntri.e2','ntri.bc.e2','ntri.be.e2','ntri.b10.e2') := as.factor(0)]


# get subsample of initial adoptions
all.adopts2 <- all.adopts.full[cum.adopt == 0 & hasboth == 1]

saveRDS(all.adopts.full,'Rds/weekly-all-adopts.Rds')
saveRDS(all.adopts2,'Rds/weekly-short-adopts.Rds')

# hard bork
stopifnot(FALSE)



# ANALYSIS TIME???
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

# all.adopts.full[, c('oddball5','oddball10','oddball15','oddball20') := 
#             list(as.numeric(rank > 5),
#                  as.numeric(rank > 10),
#                  as.numeric(rank > 15),
#                  as.numeric(rank > 20))]

# have to consider alter 0 trade days separately...
# all.adopts.full[, ntaltGT0 := as.numeric(ntotal.a14 > 0)]

# trichotomize alter, self results
# all.adopts.full[, ntri.a14 := as.factor(sign(npos.a14 - nneg.a14))]
# all.adopts.full[ntri.a14 == 0, ntri.a14 := as.factor(as.numeric(ntotal.a14>0))]

# trichotomize with benchmark
# all.adopts.full[, winfrac.e2 := npos.e2 / ntotal.e2]
# all.adopts.full[, ntri.e2 := as.factor(sign(winfrac.e2 - 0.5))]
# all.adopts.full[, ntri.bc.e2 := as.factor(sign(winfrac.e2 - all.winfrac))]
# all.adopts.full[, ntri.be.e2 := as.factor(sign(winfrac.e2 - user.winfrac))]
# all.adopts.full[, ntri.b10.e2 := as.factor(sign(winfrac.e2 - winfrac.e10))]
# all.adopts.full[ntotal.e2 == 0,
#                 c('ntri.e2','ntri.bc.e2','ntri.be.e2','ntri.b10.e2') := as.factor(0)]


# get subsample of initial adoptions
# all.adopts2 <- all.adopts.full[cum.adopt == 0 & hasboth == 1]
# 
# saveRDS(all.adopts.full,'Rds/weekly-all-adopts.Rds')
# saveRDS(all.adopts2,'Rds/weekly-short-adopts.Rds')

# ANALYSIS TIME???
# m1 <- clogit(badopt ~ ntotal.a14 + strata(grp), data = all.adopts2)
# print(summary(m1))
# m2 <- clogit(badopt ~ ntotal.a14 + npos.a14 + strata(grp), data = all.adopts2)
# print(summary(m2))
# m3 <- clogit(badopt ~ ntotal.a14*npos.a14 + strata(grp), data = all.adopts2)
# print(summary(m3))
# 
# me1 <- clogit(badopt ~ ntotal.a14*npos.e2 + npos.a14*npos.e2 + strata(grp), data = all.adopts2)
# print(summary(me1))
# me2 <- clogit(badopt ~ ntotal.a14*npos.a14*npos.e2 + strata(grp), data = all.adopts2)
# print(summary(me2))
# 
# mr1 <- clogit(badopt ~ ntotal.a14*rank + npos.a14*rank + strata(grp), data = all.adopts2)
# print(summary(mr1))
# mr2 <- clogit(badopt ~ ntotal.a14*npos.a14*rank + strata(grp), data = all.adopts2)
# print(summary(mr2))
# 
# mcr1 <- clogit(badopt ~ ntotal.a14*oddball10 + npos.a14*oddball10 + strata(grp), data = all.adopts2)
# print(summary(mcr1))
# mcr2 <- clogit(badopt ~ ntotal.a14*npos.a14*oddball10 + strata(grp), data = all.adopts2)
# print(summary(mcr2))
# 
# 
# # consider alter 0-returns separately...
# m1a <- clogit(badopt ~ ntaltGT0 + ntotal.a14 + strata(grp), data = all.adopts2)
# print(summary(m1a))
# m2a <- clogit(badopt ~ ntaltGT0 + ntotal.a14 + npos.a14 + strata(grp), data = all.adopts2)
# print(summary(m2a))
# m3a <- clogit(badopt ~ ntaltGT0 + ntotal.a14*npos.a14 + strata(grp), data = all.adopts2)
# print(summary(m3a))
# 
# me1a <- clogit(badopt ~ ntaltGT0*npos.e2 + ntotal.a14*npos.e2 + npos.a14*npos.e2 + strata(grp), data = all.adopts2)
# print(summary(me1a))
# me2a <- clogit(badopt ~ ntaltGT0*npos.e2 + ntotal.a14*npos.a14*npos.e2 + strata(grp), data = all.adopts2)
# print(summary(me2a))
# 
# mr1a <- clogit(badopt ~ ntaltGT0*rank + ntotal.a14*rank + npos.a14*rank + strata(grp), data = all.adopts2)
# print(summary(mr1a))
# mr2a <- clogit(badopt ~ ntaltGT0*rank + ntotal.a14*npos.a14*rank + strata(grp), data = all.adopts2)
# print(summary(mr2a))
# 
# mcr1a <- clogit(badopt ~ ntaltGT0*oddball10 + ntotal.a14*oddball10 + npos.a14*oddball10 + strata(grp), data = all.adopts2)
# print(summary(mcr1a))
# mcr2a <- clogit(badopt ~ ntaltGT0*oddball10 + ntotal.a14*npos.a14*oddball10 + strata(grp), data = all.adopts2)
# print(summary(mcr2a))
# 
# 
# 
       caption='Conditional Logit: Currency Pair Adoption with Rank Interaction',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=c(8,9,1,3,2,4,5,6,7),
       custom.coef.names=c('Alter Total (14d)','Currency Rank','Alter Wins (14d)',
                           'Alter Total:Rank','Alter Wins:Rank',
                           'Alter Total:Wins','Alter Wins:Rank','Alter Total:Alter Wins:Rank',
                           'Alter Trades > 0','Alter Trades > 0:Rank',
                           'Alter Total:Rank','Alter Total:Alter Wins:Rank'))

texreg(list(mcr1,mcr2, mcr1a, mcr2a),
       file='adopts-coarserank.tex', label='tab:adopt-coarserank', 
       digits=5, float.pos='htb',
       caption='Conditional Logit: Currency Pair Adoption with Coarse Rank Interaction',
       reorder.coef=c(8,9,1,3,2,4,5,6,7),
       custom.coef.names=c('Alter Total (14d)','Oddball Currency (Rank > 10)','Alter Wins (14d)',
                           'Alter Total:Oddball','Alter Wins:Oddball',
                           'Alter Total:Wins','Alter Wins:Oddball','Alter Total:Alter Wins:Oddball',
                           'Alter Trades > 0','Alter Trades > 0:Oddball',
                           'Alter Total:Oddball','Alter Total:Alter Wins:Oddball'))
