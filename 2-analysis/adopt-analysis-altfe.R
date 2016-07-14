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
    OUT.DIR <- '~/Dropbox/forex Project/writing/'
    CODE.DIR <- '~/Dropbox/forex Project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)


# reload session
if(FALSE) {
    load('adopt-analysis.Rdata')
}


ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

good.cols <- c('user_id','day','cp',
               'ugrp','grp',
               #'ntotal.e2','npos.e2','nneg.e2','nfr.e2',
               #'ntotal.e10','npos.e10','nneg.e10','nfr.e10',
               #'ntotal.a14','npos.a14','nneg.a14','nfr.a14',
               #'daysactive','all.winfrac','user.winfrac','winfrac.e10',
               'badopt','rank',
               'oddball5','oddball10','oddball20',
               'ntgt0.a14','ndpos.a14'
               #,'ndpos.bc.a14'
               )
#select.max <- 5000
#select.max <- 20000

#print(system.time(
        #all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_grp_select <= 5000),]))
    #))

# drop observations after adoption
#all.adopts <- all.adopts[cum.adopt==0]

print(system.time(
        #all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_grp_select <= select.max & cum.adopt == 0), good.cols]))
        all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, cum.adopt == 0), good.cols]))
    ))


################################################################################
################################################################################
# ANALYSIS TIME
################################################################################
################################################################################

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

