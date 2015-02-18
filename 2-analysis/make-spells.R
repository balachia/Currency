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

ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

# spell splits:
# - new user
# - new pair
# - new cumulative adoptions (i.e. split after adoption)
# - new total (i.e. split when alters start/stop trading)
# - new positive (i.e. split when alters start/stop making money)
splitsff <- ff(c(NA,head(ad.ffd$user_id,-1))) != ad.ffd$user_id
cpint <- clone(ad.ffd$cp)
virtual(cpint)$ramclass <- NULL
virtual(cpint)$Levels <- NULL
splitsff <- splitsff + (ff(c(NA,head(cpint,-1))) != cpint)
splitsff <- splitsff + (ff(c(NA,head(ad.ffd$cum.adopt,-1))) != ad.ffd$cum.adopt)
splitsff <- splitsff + (ff(c(NA,head(ad.ffd$ntgt0.a14,-1))) != ad.ffd$ntgt0.a14)
splitsff <- splitsff + (ff(c(NA,head(ad.ffd$ndpos.a14,-1))) != ad.ffd$ndpos.a14)

splitsff[1] <- 1
bsplitsff <- splitsff > 0
sum(splitsff,na.rm=TRUE)
sum(bsplitsff,na.rm=TRUE)

ffidx <- ffwhich(splitsff, splitsff > 0)
endidx <- ff(c(tail(ffidx,-1)-1,nrow(ad.ffd)), vmode='integer')



good.cols <- c('user_id','day','cp',
               'ugrp','ugrpN','grp','grpN',
               'cum.adopt','rank',
               'oddball5','oddball10','oddball20',
               'nopen.ego','nopen.alt','nopen.all',
               'ntgt0.a14','ndpos.a14','ndpos.bc.a14'
               )
print(system.time(adopt.spells <- ad.ffd[ffidx,good.cols]))
adopt.spells$badopt <- ad.ffd$badopt[endidx]
adopt.spells$endday <- ad.ffd$day[endidx]
print(system.time(
    adopt.spells <- as.data.table(as.data.frame(adopt.spells))))
adopt.spells[,days := endday - day + 1]

saveRDS(adopt.spells,'Rds/adoption-spells.Rds', compress=FALSE)


