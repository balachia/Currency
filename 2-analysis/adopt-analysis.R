library(data.table)
library(ffbase)
library(texreg)
library(survival)
library(brglm)
library(elrm)
library(stringr)



rm(list=ls())

# MATCH FILE SETTINGS TO HOST MACHINE
MC.CORES <- 2
if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/forex/'
    DATA.DIR <- '~/Data/forex/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
    
    # yen match
    yenre <- str_match(Sys.info()[['nodename']], 'yen(\\d)\\.stanford\\.edu')
    if (yenre[1,2] == '5') {
        MC.CORES <- 60
    } else if (yenre[1,2] == '6' | yenre[1,2] == '7') {
        MC.CORES <- 12
    }
} else {
    DATA.DIR <- '~/Data/forex/'
    OUT.DIR <- '~/Dropbox/forex project/writing/'
    CODE.DIR <- '~/Dropbox/forex project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)


# reload session
if(FALSE) {
    load('adopt-analysis.Rdata')
}


ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd
tl.users <- readRDS('Rds/trade-leader-users.Rds')

good.cols <- c('user_id','day','cp',
               'ugrp','ugrpN','grp','grpN',
               #'ntotal.e2','npos.e2','nneg.e2','nfr.e2',
               #'ntotal.e10','npos.e10','nneg.e10','nfr.e10',
               #'ntotal.a14','npos.a14','nneg.a14','nfr.a14',
               #'daysactive','all.winfrac','user.winfrac','winfrac.e10',
               'cum.adopt','badopt','rank',
               'oddball5','oddball10','oddball20',
               'nopen.ego','nopen.alt','nopen.all',
               'ntgt0.a14','ndpos.a14','ndpos.bc.a14'
               )
select.max <- 5000
#select.max <- 10000
select.max <- 20000

#print(system.time(
        #all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_grp_select <= 5000),]))
    #))

# drop observations after adoption
#all.adopts <- all.adopts[cum.adopt==0]

print(system.time(
        all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_grp_select <= select.max & cum.adopt == 0), good.cols]))
    ))

all.adopts[, oddball25 := rank > 25]
all.adopts[, oddball30 := rank > 30]
all.adopts[, oddball40 := rank > 40]
all.adopts[, oddball50 := rank > 50]
all.adopts[, nopen.other := nopen.all - nopen.ego - nopen.alt]
all.adopts[, nopen.eq0 := as.numeric(nopen.other==0)]

# normalize by weekly activity in that currency
all.adopts[order(day),
           nopen.week := filter(nopen.other,
                                filter=rep(1,min(7,.N)),
                                sides=1,
                                method='conv'),
           by=list(user_id,cp)]
all.adopts[order(day),
           nopen.week := c(rep(sum(nopen.other[1:7],na.rm=TRUE),6),tail(nopen.week,-6)),
           by=list(user_id,cp)]
all.adopts[, nopen.week := as.numeric(nopen.week)]
all.adopts[, nopen.norm := nopen.other / (nopen.week + 1)]

# normalize by activity across all currencies
all.adopts[, nopen.week.all := sum(nopen.week), by=list(user_id,day)]
all.adopts[, nopen.norm.all := nopen.other / (nopen.week.all + 1)]

# merge in trade leader users
all.adopts <- merge(all.adopts,
                    tl.users[,list(user_id=tl_id,tl.user)],
                    by='user_id', all.x=TRUE)
all.adopts[is.na(tl.user),tl.user := 0]

# save data file
save(all.adopts,file='Rdata/adopt-analysis-data.Rdata',compress=FALSE)

if(FALSE) {
    load('Rdata/adopt-analysis-data.Rdata')
}

#all.adopts.full <- readRDS('Rds/weekly-all-adopts.Rds')
#all.adopts2 <- readRDS('Rds/weekly-short-adopts.Rds')
# all.adopts2 <- readRDS('Rds/weekly-short-adopts.old.Rds')

# add in alter community benchmarking...
# all.adopts2[, ntri.bc.a14 := sign((npos.a14 / ntotal.a14) - all.winfrac)]
# all.adopts2[ntotal.a14 == 0, ntri.bc.a14 := 0]

# we've got some annoying collinearity here:
# all.adopts2[, ndpos.a14 := as.numeric(ntri.a14 == 1)]
# all.adopts2[, ndpos.bc.a14 := as.numeric(ntri.bc.a14 == 1)]

#all.adopts.full[, ntri.a14 := as.factor(ntri.a14)]
# all.adopts2[, ntri.a14 := as.factor(ntri.a14)]
# all.adopts2[, ntri.bc.a14 := as.factor(ntri.bc.a14)]


# add time
# all.adopts2[, year := year(as.Date(day, origin='1970-01-01'))]
# all.adopts2[, month := month(as.Date(day, origin='1970-01-01'))]
# all.adopts2[, week := week(as.Date(day, origin='1970-01-01'))]

# all.adopts2[, tgrp := year*12 + month]


#all.adopts.full.act <- all.adopts.full[user.active == 1]
# all.adopts2.act <- all.adopts2[user.active == 1]

# all.adopts2[, elrm1 := 1]

################################################################################
################################################################################
# ANALYSIS TIME
################################################################################
################################################################################

################################################################################
# BASELINE
print(system.time(basem1 <- clogit(badopt ~ ntgt0.a14 + strata(grp), data = all.adopts)))
print(summary(basem1))

print(system.time(basem2 <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + strata(grp), data = all.adopts)))
print(summary(basem2))

print(system.time(basem3 <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + strata(grp), data = all.adopts)))
print(summary(basem3))

save(basem1,basem2,basem3,
     file='Rdata/adopt-analysis-basem.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem[123]',ls())]))
gc()


################################################################################
# CNN EFFECT
print(system.time(basem1.f <- clogit(badopt ~ ntgt0.a14 * nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem1.f))

print(system.time(basem2.f <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem2.f))

print(system.time(basem3.f <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem3.f))

# alt spec - log
print(system.time(basem1.lf <- clogit(badopt ~ ntgt0.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem1.lf))

print(system.time(basem2.lf <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem2.lf))

print(system.time(basem3.lf <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem3.lf))

# alt spec - poly
print(system.time(basem1.pf <- clogit(badopt ~ ntgt0.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem1.pf))

print(system.time(basem2.pf <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem2.pf))

print(system.time(basem3.pf <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem3.pf))

save(basem1.f,basem2.f,basem3.f,
     basem1.lf,basem2.lf,basem3.lf,
     basem1.pf,basem2.pf,basem3.pf,
     file='Rdata/adopt-analysis-cnn.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem[123]',ls())]))
gc()


################################################################################
# PSEUDO-COX: USER STRATA



################################################################################
# PSEUDO-COX: USER STRATA



################################################################################
# PSEUDO-COX: USER STRATA



################################################################################
# RANK SPECIFICATIONS

print(system.time(basem4 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(grp), data = all.adopts)))
print(summary(basem4))

print(system.time(basem4.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.l))

# b-splines
print(system.time(basem4.bs0 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.bs0))

print(system.time(basem4.bs1 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank, knots=10) + strata(grp), data = all.adopts)))
print(summary(basem4.bs1))

print(system.time(basem4.bs2 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank, knots=c(5,25)) + strata(grp), data = all.adopts)))
print(summary(basem4.bs2))

# natural splines
print(system.time(basem4.ns0 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.ns0))

print(system.time(basem4.ns1 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank, knots=10) + strata(grp), data = all.adopts)))
print(summary(basem4.ns1))

print(system.time(basem4.ns2 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank, knots=c(5,25)) + strata(grp), data = all.adopts)))
print(summary(basem4.ns2))

save(basem4,basem4.l,
     basem4.bs0,basem4.bs1,basem4.bs2,
     basem4.ns0,basem4.ns1,basem4.ns2,
     file='Rdata/adopt-analysis-rank.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem4',ls())]))
gc()


################################################################################
# PSEUDO-COX: USER STRATA



################################################################################
# ODDBALLS

print(system.time(basem5.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5))

print(system.time(basem5.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10))

print(system.time(basem5.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20))

save(basem5.5,basem5.10,basem5.20,
     file='Rdata/adopt-analysis-oddball.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem5',ls())]))
gc()


################################################################################
# PSEUDO-COX: USER STRATA



################################################################################
# ODDBALLS + CNN
print(system.time(basem5.5f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))

# log friends
print(system.time(basem5.5lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))

# poly friends
print(system.time(basem5.5pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))

save(basem5.5f,basem5.10f,basem5.20f,
     basem5.5lf,basem5.10lf,basem5.20lf,
     basem5.5pf,basem5.10pf,basem5.20pf,
     file='Rdata/adopt-analysis-oddball-cnn.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem5',ls())]))
gc()


################################################################################
# DOUBLE ODDBALLS
print(system.time(basem6 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*(oddball5 + oddball25) + strata(grp), data = all.adopts)))
print(summary(basem6))

save(basem6,
     file='Rdata/adopt-analysis-double-oddball.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem6',ls())]))
gc()


################################################################################
# CURRENCY-PAIR FEs

print(system.time(basem7 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(grp,cp), data = all.adopts)))
print(summary(basem7))

print(system.time(basem7.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(grp,cp), data = all.adopts)))
print(summary(basem7.l))

print(system.time(basem7.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(grp,cp), data = all.adopts)))
print(summary(basem7.5))

print(system.time(basem7.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(grp,cp), data = all.adopts)))
print(summary(basem7.10))

print(system.time(basem7.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(grp,cp), data = all.adopts)))
print(summary(basem7.20))

save(basem7,basem7.l,basem7.5,basem7.10,basem7.20,
     file='Rdata/adopt-analysis-time-fes.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem7',ls())]))
gc()


################################################################################
# STRAIGHT CURRENCY FEs
# these appear to be unviable -- too much memory use, too slow

#print(system.time(basem8 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + as.factor(cp) + strata(grp), data = all.adopts)))
#print(summary(basem8))

#print(system.time(basem8.full <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*as.factor(cp) + strata(grp), data = all.adopts)))
#print(summary(basem8.full))


################################################################################
# CURRENCY OPENS

#print(system.time(basem9 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + nopen.norm.all + nopen.eq0 + strata(grp), data = all.adopts)))
#print(summary(basem9))

#print(system.time(basem9.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + nopen.norm.all + nopen.eq0 + strata(grp), data = all.adopts)))
#print(summary(basem9.l))

#print(system.time(basem9.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + nopen.norm.all + nopen.eq0 + strata(grp), data = all.adopts)))
#print(summary(basem9.5))

#print(system.time(basem9.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + nopen.norm.all + nopen.eq0 + strata(grp), data = all.adopts)))
#print(summary(basem9.10))

#print(system.time(basem9.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + nopen.norm.all + nopen.eq0 + strata(grp), data = all.adopts)))
#print(summary(basem9.20))

# interact with rank
print(system.time(basem9a <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*rank + strata(grp), data = all.adopts)))
print(summary(basem9a))

print(system.time(basem9a.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*log(rank) + strata(grp), data = all.adopts)))
print(summary(basem9a.l))

print(system.time(basem9a.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem9a.5))

print(system.time(basem9a.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem9a.10))

print(system.time(basem9a.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem9a.20))

#save(basem9,basem9.l,basem9.5,basem9.10,basem9.20,
     #basem9a,basem9a.l,basem9a.5,basem9a.10,basem9a.20,
     #file='Rdata/adopt-analysis-control-opens.Rdata',compress=FALSE)

save(basem9a,basem9a.l,basem9a.5,basem9a.10,basem9a.20,
     file='Rdata/adopt-analysis-control-opens.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem9',ls())]))
gc()


################################################################################
# PSEUDO-COX: USER STRATA

print(system.time(basem10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(user_id,day), data = all.adopts)))
print(summary(basem10))

print(system.time(basem10.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(user_id,day), data = all.adopts)))
print(summary(basem10.l))

print(system.time(basem10.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(user_id,day), data = all.adopts)))
print(summary(basem10.5))

print(system.time(basem10.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(user_id,day), data = all.adopts)))
print(summary(basem10.10))

print(system.time(basem10.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(user_id,day), data = all.adopts)))
print(summary(basem10.20))

save(basem10,basem10.l,basem10.5,basem10.10,basem10.20,
     file='Rdata/adopt-analysis-sudocox-user.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem10',ls())]))
gc()


################################################################################
# PSEUDO-COX: CURRENCY STRATA

print(system.time(basem11 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(cp,day), data = all.adopts)))
print(summary(basem11))

print(system.time(basem11.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(cp,day), data = all.adopts)))
print(summary(basem11.l))

print(system.time(basem11.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(cp,day), data = all.adopts)))
print(summary(basem11.5))

print(system.time(basem11.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(cp,day), data = all.adopts)))
print(summary(basem11.10))

print(system.time(basem11.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(cp,day), data = all.adopts)))
print(summary(basem11.20))

save(basem11,basem11.l,basem11.5,basem11.10,basem11.20,
     file='Rdata/adopt-analysis-sudocox-cp.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem11',ls())]))
gc()


################################################################################
# TRADE LEADER EXPERIENCE

print(system.time(basem12 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*rank + strata(grp), data = all.adopts)))
print(summary(basem12))

print(system.time(basem12.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*log(rank) + strata(grp), data = all.adopts)))
print(summary(basem12.l))

print(system.time(basem12.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem12.5))

print(system.time(basem12.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem12.10))

print(system.time(basem12.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem12.20))

# sudo-cox
print(system.time(basem12a <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*rank + strata(user_id,day), data = all.adopts)))
print(summary(basem12a))

print(system.time(basem12a.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*log(rank) + strata(user_id,day), data = all.adopts)))
print(summary(basem12a.l))

print(system.time(basem12a.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball5 + strata(user_id,day), data = all.adopts)))
print(summary(basem12a.5))

print(system.time(basem12a.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball10 + strata(user_id,day), data = all.adopts)))
print(summary(basem12a.10))

print(system.time(basem12a.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*tl.user*oddball20 + strata(user_id,day), data = all.adopts)))
print(summary(basem12a.20))

# with currency opens
print(system.time(basem12b <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*tl.user*rank + strata(user_id,day), data = all.adopts)))
print(summary(basem12b))

print(system.time(basem12b.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*tl.user*log(rank) + strata(user_id,day), data = all.adopts)))
print(summary(basem12b.l))

print(system.time(basem12b.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*tl.user*oddball5 + strata(user_id,day), data = all.adopts)))
print(summary(basem12b.5))

print(system.time(basem12b.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*tl.user*oddball10 + strata(user_id,day), data = all.adopts)))
print(summary(basem12b.10))

print(system.time(basem12b.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nopen.norm.all + nopen.eq0)*tl.user*oddball20 + strata(user_id,day), data = all.adopts)))
print(summary(basem12a.20))

save(basem12,basem12.l,basem12.5,basem12.10,basem12.20,
     file='Rdata/adopt-analysis-tl.Rdata',compress=FALSE)

save(basem12a,basem12a.l,basem12a.5,basem12a.10,basem12a.20,
     file='Rdata/adopt-analysis-tl-sudocox.Rdata',compress=FALSE)

save(basem12b,basem12b.l,basem12b.5,basem12b.10,basem12b.20,
     file='Rdata/adopt-analysis-tl-sudocox-opens.Rdata',compress=FALSE)

do.call(rm,as.list(ls()[grep('basem12',ls())]))
gc()


################################################################################
# SAVE MODELS
save.image(file='adopt-analysis.Rdata', compress=FALSE)

# hard bork
stopifnot(FALSE)

## PLOTS
png('adopt-rank-ecdf.png', width=1600, height=900, pointsize = 12, res=300)
ggplot(all.adopts[badopt==1], aes(rank)) + 
    stat_ecdf() + labs(x=NULL, y=NULL)
dev.off()

# ELRM TEST
# print(system.time(bm1e <- elrm(badopt/elrm1 ~ ntaltGT0 + ntotal.a14 + factor(grp),
#                                interest = ~ ntaltGT0 + ntotal.a14, dataset = all.adopts2)))
# print(summary(bm1e))
# print(system.time(bm1b <- brglm(badopt ~ ntaltGT0 + ntotal.a14 + factor(grp), data = all.adopts2)))
# print(summary(bm1b))

# community bechmark + oddballs
print(system.time(mabrtw1 <- clogit(badopt ~ ntaltGT0*rank + ndpos.bc.a14*rank + strata(grp), data = all.adopts2)))
print(summary(mabrtw1))
print(system.time(mabrtw2 <- clogit(badopt ~ ntaltGT0*poly(rank,2,raw=TRUE) + ndpos.bc.a14*poly(rank,2,raw=TRUE) + strata(grp), data = all.adopts2)))
print(summary(mabrtw2))
print(system.time(mabrtw3 <- clogit(badopt ~ ntaltGT0*oddball10 + ndpos.bc.a14*oddball10 + strata(grp), data = all.adopts2)))
print(summary(mabrtw3))


# other shit...
matw1a <- clogit(badopt ~ ntaltGT0*ntri.e2 + ntri.a14*ntri.e2  + strata(grp), data = all.adopts2)
print(summary(matw1a))
matw2a <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.e2  + strata(grp), data = all.adopts2)
print(summary(matw2a))



# time group stuff
print(system.time(mabtw1tg <- clogit(badopt ~ ntaltGT0 + ndpos.bc.a14  + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabtw1tg))
print(system.time(mabrtw1tg <- clogit(badopt ~ ntaltGT0*rank + ndpos.bc.a14*rank + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw1tg))
print(system.time(mabrtw2tg <- clogit(badopt ~ ntaltGT0*poly(rank,2,raw=TRUE) + ndpos.bc.a14*poly(rank,2,raw=TRUE) + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw2tg))
print(system.time(mabrtw3tg <- clogit(badopt ~ ntaltGT0*oddball10 + ndpos.bc.a14*oddball10 + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw3tg))



# let's test... active users
matw1.act <- clogit(badopt ~ ntaltGT0 + ntri.a14  + strata(grp), data = all.adopts2.act)
print(summary(matw1.act))
matw2.act <- clogit(badopt ~ ntaltGT0*ntri.a14  + strata(grp), data = all.adopts2.act)
print(summary(matw2.act))

matw1a.act <- clogit(badopt ~ ntaltGT0*ntri.e2 + ntri.a14*ntri.e2  + strata(grp), data = all.adopts2.act)
print(summary(matw1a.act))
matw2a.act <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.e2  + strata(grp), data = all.adopts2.act)
print(summary(matw2a.act))



# benchmarking success?
mtw1.be <- clogit(badopt ~ ntaltGT0 + ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(mtw1.be))
mtw2.be <- clogit(badopt ~ ntaltGT0*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(mtw2.be))

matw1a.be <- clogit(badopt ~ ntaltGT0*ntri.be.e2 + ntri.a14*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(matw1a.be))
matw2a.be <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(matw2a.be))



# re-entry?
all.adopts.full[,badopt.full := ifelse(badopt2 == 1 & uselapse >= 60,1,0)]

matw1f <- clogit(badopt.full ~ ntaltGT0 + ntri.a14  + strata(grp), data = all.adopts.full)
print(summary(matw1f))
matw2f <- clogit(badopt.full ~ ntaltGT0*ntri.a14  + strata(grp), data = all.adopts.full)
print(summary(matw2f))



# write out some results...
setwd(paste0(OUT.DIR,'tables/'))

texreg(list(mabtw1,mabrtw3,mabrtw1,mabrtw2),
       file='adopts-dummies.tex', label='tab:adopt-dummies',
       digits=4, float.pos='htb',
       caption='Conditional Logit on Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=c('Alter: Any Trades','Alter: > Benchmark',
                           'Oddball Currency','Any Trades:Oddball','> Benchmark:Oddball',
                           'Currency Rank','Any Trades:Rank','> Benchmark:Rank',
                           'Currency Rank','Rank^2','Any Trades:Rank','Any Trades:Rank^2','> Benchmark:Rank','> Benchmark:Rank^2'
                           ))

texreg(list(mtw1,mtw2),
       file='adopts-tw.tex', label='tab:adopt-triwin',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Ego Tri-Wins',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1,matw2),
       file='adopts-atw.tex', label='tab:adopt-atriwin',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Alter Tri-Wins',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1a,matw2a),
       file='adopts-twinter.tex', label='tab:adopt-triwin-inter',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Tri-Win Interactions',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1.act,matw2.act),
       file='adopts-active.tex', label='tab:adopt-active',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Exclude Inactive Users (No activity in 60 days)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1.be,matw2.be),
       file='adopts-benchego.tex', label='tab:adopt-benchego',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Benchmark vs All Own Results',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

