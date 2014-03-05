library(ffbase)
library(data.table)
library(lubridate)



# MATCH FILE SETTINGS TO HOST MACHINE
if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/Currensee/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
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

# reduce users under consideration
c.dt <- dt[user_id %in% uids]



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

