library(data.table)

rm(list=ls())

qcuts <- function(x, qs) {
    cut(x, unique(quantile(x, qs, na.rm=TRUE)), labels=FALSE, include.lowest=TRUE)
}

setwd('~/Data/Currensee/')

# load in data
fpt <- readRDS('forexposition.Rds')
dbap <- readRDS('dailybrokeraccount.Rds')
ld <- readRDS('linkdata.Rds')

# drop pandas index
# fpt[,X:=NULL]
# ld[,X:=NULL]

# make link data undirected
# ld2 <- copy(ld)
# setnames(ld2, names(ld2)[1:2], names(ld2)[2:1])
# ld <- rbind(ld, ld2, use.names=TRUE)
ld[,day := floor(senddate / 86400000)]
# setnames(ld,'senderid','user_id')
setkey(ld,user_id,day)

# make days
fpt[,c('openday','closeday') := list(
    floor((opendate / 86400000) + 0.125),
    floor((closedate / 86400000) + 0.125)
)]
dbap[,date:=day]
dbap[,day:=ceiling(date / 86400000)]


# censor to valid days, jan 1, 2008
g.min.day <- 1199145600000 / 86400000
g.max.day <- 16072
fpt <- fpt[openday >= g.min.day]
dbap <- dbap[day >= g.min.day]

# get active users and user stats
# restrict to active broker accounts...
baids <- fpt[,1,by=brokerAccount_id][,brokerAccount_id]
users <- fpt[,list(N=.N,
                   pos_dpnl= sum(dollarpnl[dollarpnl>0]),
                   neg_dpnl= -sum(dollarpnl[dollarpnl<0])
                   ),by=user_id]

dbap <- dbap[brokerAccount_id %in% baids]
user.stats <- dbap[,list(naccts=.SD[,length(unique(brokerAccount_id))],
                         netdep=sum(netDeposits[netDeposits>0]),
                         max_ob=max(.SD[,sum(openBalance),by=day][,V1]),
                         min_ob=min(.SD[,sum(openBalance),by=day][,V1]),
                         mean_ob=mean(.SD[,sum(openBalance),by=day][,V1]),
                         med_ob=median(.SD[,sum(openBalance),by=day][,V1]),
                         minday=min(day),
                         maxday=max(day)
                         ),by=user_id]

user.stats[,dayson:=maxday-minday+1]

# merge in num of friends
setkey(users,user_id)
ld2 <- ld[,list(nfriends=.N),by=user_id]
setkey(ld2,user_id)

users <- merge(users,ld2,all.x=TRUE)
users[is.na(nfriends), nfriends := 0]

# merge in dbap stats
setkey(user.stats,user_id)

active.user.stats <- merge(users,user.stats)




active.user.stats[,Npd:=N/dayson]
active.user.stats[,dpnlpd:=(pos_dpnl + neg_dpnl)/dayson]

cuts <- c(0,0.05,0.275,0.5,0.725,0.95,1.0)
active.user.stats[,nfriends_q := qcuts(nfriends, cuts)]
active.user.stats[,naccts_q := qcuts(naccts, cuts)]
active.user.stats[,max_ob_q := qcuts(max_ob, cuts)]
active.user.stats[,mean_ob_q := qcuts(mean_ob, cuts)]
active.user.stats[,med_ob_q := qcuts(med_ob, cuts)]
active.user.stats[,netdep_q := qcuts(netdep, cuts)]
active.user.stats[,N_q := qcuts(N, cuts)]
active.user.stats[,Npd_q := qcuts(Npd, cuts)]
active.user.stats[,dpnlpd_q := qcuts(dpnlpd, cuts)]


# active.user.stats[,max_ob_q := cut(max_ob, breaks=quantile(max_ob,seq(0,1,0.1)), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,mean_ob_q := cut(mean_ob, breaks=quantile(mean_ob,seq(0,1,0.1)), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,med_ob_q := cut(med_ob, breaks=quantile(med_ob,seq(0,1,0.1)), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,netdep_q := cut(ndep, breaks=quantile(netdep,seq(0,1,0.1), na.rm=TRUE), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,N_q := cut(N, breaks=quantile(N,seq(0,1,0.1)), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,Npd_q := cut(Npd, breaks=quantile(Npd,seq(0,1,0.1)), labels=FALSE, include.lowest=TRUE)]
# active.user.stats[,dpnlpd_q := cut(dpnlpd, breaks=quantile(dpnlpd,seq(0,1,0.1), na.rm=TRUE), labels=FALSE, include.lowest=TRUE)]

saveRDS(active.user.stats,'active-user-quantiles.Rds')
