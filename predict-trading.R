library(plyr)
library(data.table)

dt.rbind <- function(dt1,dt2) {

}

rm(list=ls())

hostname <- Sys.info()['nodename']
if(grepl('yen|barley|corn',hostname)) {
    setwd('~/2YP/data')
} else {
    setwd('~/Data/Currensee/')
}

fpt <- readRDS('forexposition.Rds')
dbap <- readRDS('dailybrokeraccount.Rds')
ld <- readRDS('linkdata.Rds')

# drop pandas index
fpt[,X:=NULL]
ld[,X:=NULL]

# make link data undirected
ld2 <- copy(ld)
setnames(ld2, names(ld2)[1:2], names(ld2)[2:1])
ld <- rbind(ld, ld2, use.names=TRUE)

# get active users and user stats
users <- fpt[,.N,by=user_id]
user_stats <- dbap[,
    list(minday=floor(min(day/86400000)), maxday=ceiling(max(day/86400000))),
    by=user_id]
user_stats[,gap:=maxday-minday]

# make currency list
fpt[,cp:=paste0(currency1,currency2)]

# make days
fpt[,c('openday','closeday') := list(
        floor(opendate / 86400000),
        floor(closedate / 86400000)
    )]

# sample some users
set.seed(1)
users[,selector := runif(.N)]
users <- users[selector < 0.01]

# censor to minimum opening day
g.min.day <- ceiling(quantile(fpt$opendate/86400000, 0.0001))
user_stats[minday < g.min.day, minday := g.min.day]
user_stats[,gap:=maxday-minday]
fpt <- fpt[opendate / 86400000 >= g.min.day]
fpt <- fpt[!is.na(dollarpnl)]

# statistics
stats <- list(test=volume)

# start the reactor
gaps <- 5:1
resdts <- lapply(users$user_id, function(uid) {
        print(uid)
        u.minday <- user_stats[user_id==uid,minday]
        u.maxday <- user_stats[user_id==uid,maxday]

        # get alters
        u.alts <- ld[senderid == uid]

        # get user and friends' transactions
        u.fpt <- fpt[user_id==uid,]
        alts.fpt <- fpt[user_id %in% u.alts$recipientid]

        # cps <- unique(c(u.fpt[,cp],alts.fpt[,cp]))

        # resdt <- data.table(day=u.minday:u.maxday)

        # loop over days
        resdts <- lapply(u.minday:u.maxday, function(cday) {
                # print(cday)
                idx <- cday - u.minday + 1

                # hasopen <- dim(u.fpt[openday==cday])[1] > 0

                c.u.alts <- u.alts[senddate/86400000 < cday, recipientid]

                c.u.fpt <- u.fpt
                c.alts.fpt <- alts.fpt[user_id %in% c.u.alts]

                resdt <- data.table()
                for (gap in gaps) {
                    c.u.fpt <- c.u.fpt[closeday < cday & closeday >= cday - gap]
                    c.alts.fpt <- c.alts.fpt[closeday < cday & closeday >= cday - gap]
                    u.res <- c.u.fpt[,list(
                            day=cday,
                            gap=gap,
                            type='ego',
                            dpnl_sum=sum(dollarpnl),
                            dpnl_mean=mean(dollarpnl),
                            dpnl_pos=sum(dollarpnl[dollarpnl>0]),
                            dpnl_neg=sum(dollarpnl[dollarpnl<0]),
                            ntotal=.N,
                            npos=sum(dollarpnl>0),
                            nneg=sum(dollarpnl<0)
                        ),by=cp]
                    alt.res <- c.alts.fpt[,list(
                            day=cday,
                            gap=gap,
                            type='alter',
                            dpnl_sum=sum(dollarpnl),
                            dpnl_mean=mean(dollarpnl),
                            dpnl_pos=sum(dollarpnl[dollarpnl>0]),
                            dpnl_neg=sum(dollarpnl[dollarpnl<0]),
                            ntotal=.N,
                            npos=sum(dollarpnl>0),
                            nneg=sum(dollarpnl<0)
                        ),by=cp]

                    if (dim(resdt)[1]==0 && 
                        dim(u.res)[1]==0 &&
                        dim(alt.res)[1]==0) {
                        # lol
                    } else {
                        resdt <- rbind(resdt,u.res,alt.res)
                    }
                }
                resdt
            })

        resdt <- rbindlist(resdts)
        print(dim(resdt))
        
        if (dim(resdt)[1] > 0) {
            resdt[,user_id:=uid]
        }
        resdt
    })
