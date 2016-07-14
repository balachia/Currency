library(data.table)

setwd('~/Data/forex/tables-2014/Rds')

rm(list=ls())

ba <- readRDS('BrokerAccount.Rds')
ba <- ba[,list(brokerAccount_id=id, user_id, baseCurrency)]
setkey(ba,brokerAccount_id)

# make fpt
fpt <- readRDS('ForexPositionTracker.Rds')
fpt[,c('version','correlationId','position_id','original_id','residual_id',
       'followedClose','brokerOrderId','stopEnterPrice','rollovers',
       'dollarPnlExclusions','pipsPnl','closeExecutionCount',
       'followedTrade_id','openBrokerTradePositionId','closeBrokerTradePositionId',
       'openPersistTime','closePersistTime') := NULL]
fpt <- fpt[status=='CLOSED']
gc()

setnames(fpt, c('openPrice','openTime','closePrice','closeTime',
           'dollarPnl','tradePnl','openBalance'),
         c('openprice','opendate','closeprice','closedate',
           'dollarpnl','tradepnl','openbalance'))
fpt[,longtr := 2 * as.numeric(direction=='LONG') - 1]
fpt[,clopdiff := closeprice - openprice]
fpt[,pctreturn := (clopdiff / openprice) * longtr]
fpt <- fpt[!is.na(pctreturn)]

# split out follower trades
fpt.f <- fpt[!is.na(openCause_id) | !is.na(closeCause_id)]
fpt <- fpt[is.na(openCause_id) & is.na(closeCause_id)]
fpt <- fpt[closureReason == '']
fpt.f[is.na(openCause_id), openCause_id := closeCause_id]
setkey(fpt,id)

fpt.f2 <- fpt.f[,list(followers = .N), by=list(id=openCause_id)]
setkey(fpt.f2,id)

fpt <- merge(fpt,fpt.f2,all.x=TRUE)
fpt[,indeptrade := as.numeric(is.na(followers))]

# merge in user ids
setkey(fpt,brokerAccount_id)
fpt <- merge(fpt,ba,all.x=TRUE)

# save and clean
saveRDS(fpt,'../../forexposition.Rds')
rm(fpt, fpt.f, fpt.f2); gc()



# make dbap
dbap <- readRDS('DailyBrokerAccountPerformance.Rds')
dbap[,c('version','intraDayHigh','intraDayLow','maxDD','peakHigh','peakLow',
        'todateClosedRoi','todateNetRoi') := NULL]

# merge in user ids
setkey(dbap,brokerAccount_id)
dbap <- merge(dbap,ba,all.x=TRUE)

# save and clean
saveRDS(dbap,'../../dailybrokeraccount.Rds')
rm(dbap); gc()





# make ld
fl <- readRDS('FriendLink.Rds')
um <- readRDS('UserMessage.Rds')

fl[,c('id','version') := NULL]
um <- um[type=='FRIEND_LINK_REQUEST',list(sender_id, recipient_id, senddate=sendDate)]
setnames(fl,c('friend1_id','friend2_id'),
         c('user_id','alter_id'))
setnames(um,c('sender_id','recipient_id'),
         c('user_id','alter_id'))

# symmetrize
fl <- rbind(fl,fl[,list(user_id=alter_id, alter_id=user_id)], use.names=TRUE)
um <- rbind(um,um[,list(user_id=alter_id, alter_id=user_id, senddate)], use.names=TRUE)

# take last date by friendship
um <- um[,list(senddate=max(senddate)), by=list(user_id,alter_id)]

# merge in dates...
setkey(fl,user_id,alter_id)
setkey(um,user_id,alter_id)
ld <- merge(fl,um,all.x=TRUE)

ld <- ld[!is.na(senddate)]

# save and clean
saveRDS(ld,'../../linkdata.Rds')
rm(fl, um, ld); gc()
