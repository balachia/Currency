library(plyr)
library(parallel)
library(data.table)

rm(list=ls())

activity.by.currency <- function(c.day,u.alts,u.fpt,alts.fpt) {
    # restrict to alters as of day
    # restrict to forex positions as of day
    c.u.alts <- u.alts[senddate/86400000 < c.day, alter_id]
    c.u.fpt <- u.fpt[openday == c.day,
                     list(day=c.day,type='ego',nopen=.N),
                     by=cp]
    c.alts.fpt <- alts.fpt[user_id %in% c.u.alts & openday == c.day,
                           list(day=c.day,type='alter',nopen=.N),
                           by=cp]
    rbind(c.u.fpt,c.alts.fpt)
}

success.by.currency <- function(c.day,u.alts,u.fpt,alts.fpt,gaps) {
    # idx <- c.day - u.minday + 1

    # hasopen <- dim(u.fpt[openday==c.day])[1] > 0

    # 86,400,000 is a day in milliseconds

    # restrict to alters as of c.day
#    c.u.alts <- u.alts[senddate/86400000 < c.day, recipientid]
    c.u.alts <- u.alts[senddate/86400000 < c.day, alter_id]

    c.u.fpt <- u.fpt
    c.alts.fpt <- alts.fpt[user_id %in% c.u.alts]

    resdt <- data.table()
    for (gap in gaps) {
        c.u.fpt <- c.u.fpt[closeday < c.day & closeday >= c.day - gap]
        c.alts.fpt <- c.alts.fpt[closeday < c.day & closeday >= c.day - gap]
        u.res <- c.u.fpt[,list(
                day=c.day,
                gap=gap,
                type='ego',
                dpnl_sum=sum(dollarpnl),
                dpnl_mean=mean(dollarpnl),
                dpnl_pos=sum(dollarpnl[dollarpnl>0]),
                dpnl_neg=sum(dollarpnl[dollarpnl<0]),
                ntotal=.N,
                npos=sum(dollarpnl>0),
                nneg=sum(dollarpnl<0),
                nfr=max(nfr)
            ),by=cp]
        alt.res <- c.alts.fpt[,list(
                day=c.day,
                gap=gap,
                type='alter',
                dpnl_sum=sum(dollarpnl),
                dpnl_mean=mean(dollarpnl),
                dpnl_pos=sum(dollarpnl[dollarpnl>0]),
                dpnl_neg=sum(dollarpnl[dollarpnl<0]),
                ntotal=.N,
                npos=sum(dollarpnl>0),
                nneg=sum(dollarpnl<0),
                nfr=max(nfr)
            ),by=cp]

        if (dim(resdt)[1]==0 && 
            dim(u.res)[1]==0 &&
            dim(alt.res)[1]==0) {
            # lol
        } else {
#            resdt <- rbind(resdt,u.res,alt.res)
            resdt <- rbindlist(list(resdt,u.res,alt.res))
        }
    }
    resdt
}

day.stats <- function(c.day,u.alts,u.fpt,alts.fpt,u.dbap) {
    resdt <- data.table(day=c.day)

    dbap.stats <- u.dbap[day < c.day,list(
            nd=sum(netDeposits),
            posd=sum(netDeposits[netDeposits>0]),
            negd=sum(netDeposits[netDeposits<0]),
            ndpnl=sum(dollarPnl),
            posdpnl=sum(dollarPnl[dollarPnl>0]),
            negdpnl=sum(dollarPnl[dollarPnl<0])
        )]

    last.open.balances <- u.dbap[day < c.day, .SD[which.max(day),list(openBalance,day)], by=brokerAccount_id]

    resdt[,c(
                'deposited_today',
                'opened_today',
                'hadopen',
                'netdeposits',
                'posdeposits',
                'negdeposits',
                'totaldpnl',
                'posdpnl',
                'negdpnl',
                'openbalance'
            ) := list(
                u.dbap[day==c.day,sum(netDeposits)],
                dim(u.fpt[openday==c.day])[1],
                dim(u.fpt[openday<c.day & closeday>=c.day])[1],
                dbap.stats$nd,
                dbap.stats$posd,
                dbap.stats$negd,
                dbap.stats$ndpnl,
                dbap.stats$posdpnl,
                dbap.stats$negdpnl,
                last.open.balances[,sum(openBalance)]
            )
        ]

    resdt
}

# run settings
SAMP.FRAC <- 1
MIN.SAMP.FRAC <- 0
#FUNC.NAME <- 'day.stats'
#FUNC.NAME <- 'success.by.currency'
FUNC.NAME <- 'activity.by.currency'

# system specific settings
hostname <- Sys.info()['nodename']
if(grepl('yen|barley|corn',hostname)) {
#    setwd('~/2YP/data/')
    setwd('~/Data/forex/Rds/')
    par.cores <- detectCores()
} else {
    setwd('~/Data/forex/')
    par.cores <- 2
}

cat('settings\n')
cat(MIN.SAMP.FRAC,'\n')
cat(SAMP.FRAC,'\n')
cat(FUNC.NAME,'\n')
cat('cores: ',par.cores,'\n')

# load in data
fpt <- readRDS('forexposition.Rds')
dbap <- readRDS('dailybrokeraccount.Rds')
ld <- readRDS('linkdata.Rds')

# drop pandas index
#fpt[,X:=NULL]
#ld[,X:=NULL]

# make link data undirected
#ld2 <- copy(ld)
#setnames(ld2, names(ld2)[1:2], names(ld2)[2:1])
#ld <- rbind(ld, ld2, use.names=TRUE)

# get active users and user stats
users <- fpt[,.N,by=user_id]
user_stats <- dbap[,
    list(minday=floor(min(day/86400000)), maxday=ceiling(max(day/86400000))),
    by=user_id]
user_stats[,gap:=maxday-minday]

setkey(users,user_id)
setkey(user_stats,user_id)
users <- user_stats[users]

# clean out inactive broker accounts in dbap
# nah, later
# unclear what this would do

# sample some users
set.seed(1)
users[,selector := runif(.N)]
# users <- users[selector < SAMP.FRAC]
users <- users[selector >= MIN.SAMP.FRAC & selector < SAMP.FRAC]

ec <- ecdf(users[,user_id])

cat('dim users: ',dim(users),'\n')

# make currency list
fpt[,cp:=paste0(currency1,currency2)]

# make days, with correction for trading day
# dear god, i have daylight savings time in here
fpt[,c('openday','closeday') := list(
        floor((opendate / 86400000) + 0.125),
        floor((closedate / 86400000) + 0.125)
    )]
dbap[,date:=day]
dbap[,day:=ceiling(date / 86400000)]

# censor to minimum opening day, jan 1 2008
# max day is jan 2, 2014
# g.min.day <- ceiling(quantile(fpt$opendate/86400000, 0.0001))
g.min.day <- 1199145600000 / 86400000
g.max.day <- 16072
users[minday < g.min.day, minday := g.min.day]
users[,gap:=maxday-minday]
fpt <- fpt[openday >= g.min.day]
fpt <- fpt[!is.na(dollarpnl)]

# correct bad users:
users[,dates_imputed:=0]
users[is.na(minday) | is.na(maxday),
    c('minday','maxday','dates_imputed') := list(g.min.day,g.max.day,1)]

# build friends by day... (~100 mb)
res <- mclapply(g.min.day:g.max.day,
                mc.preschedule=FALSE, mc.cores=par.cores,
                FUN=function(cday) {
        ld[(senddate/86400000) <= cday,list(closeday=cday, nfr=.N),by=user_id]
    })
ld.by.day <- rbindlist(res)

# merge into fpt
fpt <- merge(fpt, ld.by.day, by=c('user_id','closeday'), all.x=TRUE)
fpt[ is.na(nfr), nfr := 0]

# start the reactor
# free mars

ptm <- proc.time()

gaps <- c(29,15,10:1)
gaps <- c(2:1)
#resdts <- lapply(users$user_id, function (uid) {
resdts <- mclapply(users$user_id,
    mc.preschedule=FALSE, mc.cores=par.cores,
    FUN=function(uid) {
        cat(uid, "(", ec(uid), ")\n")

        iter.ptm <- proc.time()
        
        u.minday <- users[user_id==uid,minday]
        u.maxday <- users[user_id==uid,maxday]
        u.imputed <- users[user_id==uid,dates_imputed]

        # get alters
#        u.alts <- ld[senderid == uid]
        u.alts <- ld[user_id == uid]

        # get user and friends' transactions
        u.dbap <- dbap[user_id==uid,]
        u.fpt <- fpt[user_id==uid,]
#        alts.fpt <- fpt[user_id %in% u.alts$recipientid]
        alts.fpt <- fpt[user_id %in% u.alts$alter_id]

        # loop over days
        if (FUNC.NAME ==  'success.by.currency') {
            c.func <- function(cday) success.by.currency(cday,u.alts,u.fpt,alts.fpt,gaps)
        } else if (FUNC.NAME ==  'day.stats') {
            c.func <- function(cday) day.stats(cday,u.alts,u.fpt,alts.fpt,u.dbap)
        } else if (FUNC.NAME ==  'activity.by.currency') {
            c.func <- function(cday) activity.by.currency(cday,u.alts,u.fpt,alts.fpt)
        }


        resdts <- lapply(u.minday:u.maxday,
            # function(cday) success.by.currency(cday,u.alts,u.fpt,alts.fpt,gaps))
            c.func)

        resdt <- rbindlist(resdts)

        out.time <- (proc.time() - iter.ptm)[3] / (dim(resdt)[1]/100)
        cat('dim: ',dim(resdt),'\n', out.time, '/100,', (proc.time() - ptm)[3], 'total\n')

        if (dim(resdt)[1] > 0) {
            resdt[,c('user_id','imputed') := list(uid,u.imputed)]
        }

        resdt
    })

resdtsf <- Filter(function(x) nrow(x) > 0, resdts)
resdt <- rbindlist(resdtsf)

saveRDS(resdt,paste0(FUNC.NAME,'-',MIN.SAMP.FRAC,'-',SAMP.FRAC,'samp.Rds'))
cat(paste0(FUNC.NAME,'-',MIN.SAMP.FRAC,'-',SAMP.FRAC,'samp.Rds'), '\n')
