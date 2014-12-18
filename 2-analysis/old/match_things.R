# let's try to see what happens when we match heavily on covariates

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



# get sizes of quantiles
summary(aus[, grep('_q',names(aus)), with=FALSE])

uids <- aus[Npd_q %in% 2:5 & 
                med_ob_q %in% 2:5 & 
                dpnlpd_q %in% 2:5 &
                netdep_q %in% 2:5,user_id]

# assign user group
aus <- aus[user_id %in% uids]
aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,netdep_q,Npd_q,dpnlpd_q)]
# aus[,ugrp := .GRP, by=list(nfriends_q,naccts_q,med_ob_q,Npd_q,dpnlpd_q)]
aus[,ugrpN := .N, by=ugrp]
summary(aus[,.N,by=ugrp])
quantile(aus[,.N,by=ugrp][,N], seq(0,1,0.1))

# drop single user groups
uids <- aus[ugrpN > 1, user_id]

# uids <- aus[Npd_q %in% 2:9 & 
#                 med_ob_q %in% 2:9 & 
#                 dpnlpd_q %in% 2:9,user_id]

c.dt <- dt[user_id %in% uids]
c.fpt <- fpt[user_id %in% uids]

# preprocess c.dt stuff
c.dt[,bopen:=as.integer(opened_today>0)]

c.dt <- c.dt[order(user_id,day)]

# merge in user statistics
setkey(c.dt,user_id)
setkey(aus,user_id)

c.dt <- merge(c.dt,aus)



# want to look at major open status, not just boolean open status...
open.cps <- c.fpt[,list(highrank=min(rank),lowrank=max(rank)),by=list(user_id,day=openday)]
setkey(c.dt,user_id,day)
c.dt <- merge(c.dt, open.cps, all.x=TRUE)

# make cp-rank open-trade booleans
for (i in c(5,10,20)) {
    c.dt[bopen==1, paste0(c('bh','bl'), i) := list(
        as.integer(highrank <= i), as.integer(highrank > i))]
    c.dt[bopen==0, paste0(c('bh','bl'), i) := list(0, 0)]
}


rm(dt, fpt, open.cps); gc()
# pull out relevant users from daily currency data stuff
ptm <- system.time(
    idx.uids <- ffwhich(ffd,user_id %in% uids & gap %in% c(2,6,10)))
ptm
# looking at users only, have ~ 83,474,877 (in 18 sec)

# let's see if this fits in a dt
ptm <- system.time(
    sbcdt <- as.data.table(as.data.frame(ffd[idx.uids,])))
ptm
# abt 96 sec, takes ~ 1.5 gb, 4 gb total in ram for some reason...
gc()
# now 1.98 gb, cool

# sbcdt <- sbcdt[gap %in% c(1,6,10)]
gc()

# merge in cp ranks
setkey(sbcdt,cp)
sbcdt <- merge(sbcdt,cps)
gc()



# collapse data
# look at partitions by rank
c.sbcdt <- sbcdt[,list(rows_smushed=.N,
                       ntotal=sum(ntotal),
                       npos=sum(npos),
                       nneg=sum(nneg),
                       dpnl_sum=sum(dpnl_sum),
                       dpnl_pos=sum(dpnl_pos),
                       dpnl_neg=sum(dpnl_neg)
),by=list(user_id,day,gap,type,major=major10)]
c.sbcdt[,dpnl_mean:=dpnl_sum/ntotal]

# look at partitions by rank
c2.sbcdt <- sbcdt[,list(rows_smushed=.N,
                       ntotal=sum(ntotal),
                       npos=sum(npos),
                       nneg=sum(nneg),
                       dpnl_sum=sum(dpnl_sum),
                       dpnl_pos=sum(dpnl_pos),
                       dpnl_neg=sum(dpnl_neg)
),by=list(user_id,day,gap,type)]
c2.sbcdt[,dpnl_mean:=dpnl_sum/ntotal]
rm(sbcdt); gc()


# make the oddballed one wide
c.sbcdt[,rows_smushed := NULL]
m.sbcdt <- melt(c.sbcdt, id.vars=c('user_id','day','gap','type','major'))

# rename levels
m.sbcdt[, major := ifelse(major==1,'maj','min')]
sbc.wide <- dcast.data.table(m.sbcdt, user_id + day ~ variable + type + major + gap)

rm(m.sbcdt,c.sbcdt); gc()



# make the all together one wide
c2.sbcdt[,rows_smushed := NULL]
m.sbcdt <- melt(c2.sbcdt, id.vars=c('user_id','day','gap','type'))

# rename levels
sbc.wide2 <- dcast.data.table(m.sbcdt, user_id + day ~ variable + type + gap)

rm(m.sbcdt,c2.sbcdt); gc()



# change na's and expand to correct size
c.dt.keys <- c.dt[,list(user_id,day)]

setkey(c.dt.keys,user_id,day)
setkey(c.dt,user_id,day)
setkey(sbc.wide,user_id,day)
setkey(sbc.wide2,user_id,day)

c.sbc.wide <- merge(c.dt.keys,sbc.wide, all.x=TRUE)
c.sbc.wide2 <- merge(c.dt.keys,sbc.wide2, all.x=TRUE)

# set NA's to 0, h/t Matt Dowle, https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
for (j in names(c.sbc.wide))
    set(c.sbc.wide,which(is.na(c.sbc.wide[[j]])),j,0.0)

for (j in names(c.sbc.wide2))
    set(c.sbc.wide2,which(is.na(c.sbc.wide2[[j]])),j,0.0)

wide2.names <- as.vector(outer(
    c('ntotal','npos','nneg','dpnl_sum','dpnl_pos','dpnl_neg'),
    c('ego','alter'),
    paste, sep='_'))
wide.names <- as.vector(outer(
    wide2.names, c('min','maj'), paste, sep='_'))
wide.mnames <- as.vector(outer(
    c('_ego_min', '_ego_maj', '_alter_min', '_alter_maj'),
    c(2,6,10),
    paste, sep='_'))
wide2.mnames <- as.vector(outer(
    c('_ego', '_alter'),
    c(2,6,10),
    paste, sep='_'))


# remove part from whole (i.e. subtract day 6 stats from day 10 stats...)
lapply(wide.names, function (x) {
        c.sbc.wide[,paste0(x,'_10') := get(paste0(x,'_10')) - get(paste0(x,'_6'))]
        c.sbc.wide[,paste0(x,'_6') := get(paste0(x,'_6')) - get(paste0(x,'_2'))]
        0
    })

lapply(wide2.names, function (x) {
        c.sbc.wide2[,paste0(x,'_10') := get(paste0(x,'_10')) - get(paste0(x,'_6'))]
        c.sbc.wide2[,paste0(x,'_6') := get(paste0(x,'_6')) - get(paste0(x,'_2'))]
        0
    })
# update means to reflect new totals (gonna get a bunch of nans)
lapply(wide.mnames, function (x) {
        c.sbc.wide[,paste0('dpnl_mean',x) := get(paste0('dpnl_sum',x)) / get(paste0('ntotal',x))]
        c.sbc.wide[is.na(get(paste0('dpnl_mean',x))), paste0('dpnl_mean',x) := 0]
        0
    })

lapply(wide2.mnames, function (x) {
        c.sbc.wide2[,paste0('dpnl_mean',x) := get(paste0('dpnl_sum',x)) / get(paste0('ntotal',x))]
        c.sbc.wide2[is.na(get(paste0('dpnl_mean',x))), paste0('dpnl_mean',x) := 0]
        0
    })



# merge in with main data
setkey(c.dt,user_id,day)
setkey(c.sbc.wide,user_id,day)
setkey(c.sbc.wide2,user_id,day)

c.dt <- merge(c.dt,c.sbc.wide2)
c.dt <- merge(c.dt,c.sbc.wide)

rm(c.fpt, sbc.wide, c.sbc.wide, sbc.wide2, c.sbc.wide2, c.dt.keys); gc()



# set up matching
keys <- c('user_id', 'day')
snames <- 'ugrp'

# qnames <- c('totaldpnl')
# qnames <- c('totaldpnl','ntotal_ego_10','ntotal_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_10','ntotal_ego_6','npos_ego_10','npos_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_10','ntotal_ego_6','npos_ego_10','npos_ego_6',
#             'dpnl_sum_ego_10','dpnl_sum_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_10')
# qnames <- c('totaldpnl','ntotal_ego_10','npos_ego_10')
qnames <- c('totaldpnl','ntotal_ego_10','npos_ego_10','dpnl_sum_ego_10')
# qnames <- c('totaldpnl','ntotal_ego_10','npos_ego_10','dpnl_sum_ego_10','dpnl_pos_ego_10')
# qnames <- c('totaldpnl','ntotal_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_6','npos_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_6','npos_ego_6','dpnl_sum_ego_6')
# qnames <- c('totaldpnl','ntotal_ego_6','npos_ego_6','dpnl_sum_ego_6','dpnl_pos_ego_6')
c.dt2 <- c.dt[ntotal_ego_2 > 0]
print(dim(c.dt2))
grps <- poor.cem(c.dt2, keys, snames, qnames, bkeys='user_id')

grps[,c('nobs','nuser') := list(.N, length(unique(user_id))),by=grp]
# tag unique user observations
set.seed(1)
grps[,tag := sample(1:.N,.N), by=list(grp,user_id)]

setkey(c.dt2,user_id,day)
setkey(grps,user_id,day)
c.dt2 <- merge(c.dt2,grps)

c.dt.final <- c.dt2[nuser > 1 & nobs > 1 & tag == 1]


# test $ amounts
summary(md1 <- clogit(bopen ~ scale(dpnl_mean_ego_2) + strata(grp), data=c.dt.final))
summary(md2 <- clogit(bopen ~ scale(dpnl_sum_ego_2) + strata(grp), data=c.dt.final))
summary(md3 <- clogit(bopen ~ scale(dpnl_mean_ego_2) + scale(dpnl_sum_ego_2) + strata(grp), data=c.dt.final))
summary(md4 <- clogit(bopen ~ scale(dpnl_mean_ego_2)*scale(dpnl_sum_ego_2) + strata(grp), data=c.dt.final))

# test numbers of results
summary(clogit(bopen ~ scale(ntotal_ego_2) + strata(grp), data=c.dt.final))
summary(mn1 <- clogit(bopen ~ scale(npos_ego_2) + strata(grp), data=c.dt.final))
summary(mn2 <- clogit(bopen ~ scale(npos_ego_2)*scale(ntotal_ego_2) + strata(grp), data=c.dt.final))
summary(mn3 <- clogit(bopen ~ I(npos_ego_2 / ntotal_ego_2) + strata(grp), data=c.dt.final))
summary(mn4 <- clogit(bopen ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + strata(grp), data=c.dt.final))
summary(mn5 <- clogit(bopen ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + 
                   scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   strata(grp), data=c.dt.final))
summary(clogit(bopen ~ I(nneg_ego_2 / ntotal_ego_2)*scale(nneg_ego_2) + 
                   scale(nneg_ego_2)*scale(ntotal_ego_2) + 
                   strata(grp), data=c.dt.final))

# fraction, scale and $ mean
summary(clogit(bopen ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + 
                   scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(dpnl_mean_ego_2) + strata(grp), data=c.dt.final))

# test alter fraction and scale effects
summary(mea0 <- clogit(bopen ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + 
                   scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   I(npos_alter_2 / ntotal_alter_2)*scale(npos_alter_2) + 
                   scale(npos_alter_2)*scale(ntotal_alter_2) + 
                   strata(grp), data=c.dt.final))

# test fraction and scale effect on major/minor opens
summary(mobdv1h <- clogit(bh10 ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + 
                   scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   strata(grp), data=c.dt.final))
summary(mobdv1l <- clogit(bl10 ~ I(npos_ego_2 / ntotal_ego_2)*scale(npos_ego_2) + 
                   scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   strata(grp), data=c.dt.final))

# test both fraction and scale, with major/minor results
summary(mobiv1 <- clogit(bopen ~ scale(npos_ego_maj_2)*scale(ntotal_ego_maj_2) + 
                   scale(npos_ego_min_2)*scale(ntotal_ego_min_2) + 
                   strata(grp), data=c.dt.final))
summary(mobiv2 <- clogit(bopen ~ I(npos_ego_maj_2 / ntotal_ego_maj_2)*scale(npos_ego_maj_2) + 
                   I(npos_ego_min_2 / ntotal_ego_min_2)*scale(npos_ego_min_2) + 
                   strata(grp), data=c.dt.final))
summary(mobiv3 <- clogit(bopen ~ I(npos_ego_maj_2 / ntotal_ego_maj_2)*scale(npos_ego_maj_2) + 
                   scale(npos_ego_maj_2)*scale(ntotal_ego_maj_2) + 
                   I(npos_ego_min_2 / ntotal_ego_min_2)*scale(npos_ego_min_2) + 
                   scale(npos_ego_min_2)*scale(ntotal_ego_min_2) + 
                   strata(grp), data=c.dt.final))

# test reversals:
summary(clogit(bopen ~ scale(npos_ego_2)*scale(ntotal_ego_2) + strata(grp), data=c.dt.final))
summary(clogit(bopen ~ scale(nneg_ego_2)*scale(ntotal_ego_2) + strata(grp), data=c.dt.final))

# compare ego and alter effects
summary(mea1 <- clogit(bopen ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(npos_alter_2)*scale(ntotal_alter_2) + 
                   strata(grp), data=c.dt.final))

# compare openings of currency type
summary(mobdv0h <- clogit(bh10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + strata(grp), data=c.dt.final))
summary(mobdv0l <- clogit(bl10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + strata(grp), data=c.dt.final))

summary(clogit(bopen ~ scale(npos_ego_maj_2)*scale(ntotal_ego_maj_2) + 
                   scale(npos_ego_min_2)*scale(ntotal_ego_min_2) + 
                   strata(grp), data=c.dt.final))
summary(clogit(bh10 ~ scale(npos_ego_maj_2)*scale(ntotal_ego_maj_2) + 
                   scale(npos_ego_min_2)*scale(ntotal_ego_min_2) + 
                   strata(grp), data=c.dt.final))
summary(clogit(bl10 ~ scale(npos_ego_maj_2)*scale(ntotal_ego_maj_2) + 
                   scale(npos_ego_min_2)*scale(ntotal_ego_min_2) + 
                   strata(grp), data=c.dt.final))

# let's throw in alters too
summary(mobdv2h <- clogit(bh10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                              scale(npos_alter_2)*scale(ntotal_alter_2) + 
                              strata(grp), data=c.dt.final))
summary(mobdv2l <- clogit(bl10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                              scale(npos_alter_2)*scale(ntotal_alter_2) + 
                              strata(grp), data=c.dt.final))

summary(clogit(bh10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(npos_alter_maj_2)*scale(ntotal_alter_maj_2) + 
                   scale(npos_alter_min_2)*scale(ntotal_alter_min_2) + 
                   strata(grp), data=c.dt.final))
summary(clogit(bl10 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(npos_alter_maj_2)*scale(ntotal_alter_maj_2) + 
                   scale(npos_alter_min_2)*scale(ntotal_alter_min_2) + 
                   strata(grp), data=c.dt.final))

# interact alter with own winningness
summary(clogit(bh5 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(npos_ego_2)*(scale(ntotal_alter_maj_2) + scale(ntotal_alter_min_2)) +
                   strata(grp), data=c.dt.final))
summary(clogit(bl5 ~ scale(npos_ego_2)*scale(ntotal_ego_2) + 
                   scale(npos_ego_2)*(scale(ntotal_alter_maj_2) + scale(ntotal_alter_min_2)) +
                   strata(grp), data=c.dt.final))


# write out results
setwd(paste0(OUT.DIR,'tables/'))

texreg(list(md1,md2,md3,md4),
       file='matching-dlrs.tex', label='tab:dlrs', digits=3, float.pos='htb',
       caption='Conditional Logit: Dollar Returns in Past 2 Days',
       custom.coef.names=c('Mean \\$ Returns','Total \\$ Returns','Mean:Total'))

texreg(list(mn1,mn2,mn3,mn4,mn5),
       file='matching-nums.tex', label='tab:nums', digits=3, float.pos='htb',
       caption='Conditional Logit: Number Wins in Past 2 Days',
       custom.coef.names=c('\\# Wins','\\# Total','Wins:Total','Win Frac.','Win Frac.:Wins'))

texreg(list(mobiv1,mobiv2,mobiv3),
       file='matching-oddiv.tex', label='tab:obiv', digits=3, float.pos='htb',
       caption='Conditional Logit: Predict Opening Positions by Major/Minor Results',
       custom.coef.names=c('\\# Wins (Common)', '\\# Total (Common)', '\\# Wins (Rare)', '\\# Total (Rare)',
                           'Wins:Total (Common)', 'Wins:Total (Rare)',
                           'Win Frac. (Common)', 'Win Frac. (Rare)',
                           'Win Frac.:Wins (Common)', 'Win Frac.:Wins (Rare)'),
       reorder.coef=c(1,2,5,7,9,3,4,6,8,10))

texreg(list(mobdv0h,mobdv0l,mobdv1h,mobdv1l,mobdv2h,mobdv2l),
       file='matching-odddv.tex', label='tab:obdv', digits=3, float.pos='htb',
       caption='Conditional Logit: Predict Opening Common/Rare Positions',
       custom.model.names=c('Common','Rare','Common','Rare','Common','Rare'),
       custom.coef.names=c('\\# Wins (Ego)', '\\# Total (Ego)', 'Win Frac.:Wins (Ego)',
                           'Win Frac. (Ego)', 'Win Frac.:Wins (Ego)', 
                           '\\# Wins (Alter)', '\\# Total (Alter)', 'Win Frac.:Wins (Alter)'))

texreg(list(mea1,mea0),
       file='matching-ea-wfracs.tex', label='tab:eafracs', digits=3, float.pos='htb',
       caption='Conditional Logit: Effects of Ego and Alter',
       custom.coef.names=c('\\# Wins (Ego)', '\\# Total (Ego)', '\\# Wins (Alter)', '\\# Total (Alter)',
                           'Wins:Total (Ego)', 'Wins:Total (Alter)', 'Win Frac. (Ego)', 'Win Frac. (Alter)',
                           'Win Frac.:Wins (Ego)', 'Win Frac.:Wins (Alter)'),
       reorder.coef = c(1,2,5,7,9,3,4,6,8,10))
