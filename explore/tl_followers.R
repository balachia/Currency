library(data.table)

rm(list=ls())

setwd('~/Data/forex/tables-2014/Rds')

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



# merge in user ids
setkey(fpt,brokerAccount_id)
fpt <- merge(fpt,ba,all.x=TRUE)
setkey(fpt.f,brokerAccount_id)
fpt.f <- merge(fpt.f,ba,all.x=TRUE)

# merge in trade leader ids
ref.trades <- fpt[,list(closeCause_id = id, tl_id = user_id, tl_baid = brokerAccount_id)]
setkey(ref.trades,closeCause_id)
setkey(fpt.f,closeCause_id)
fpt.f <- merge(fpt.f, ref.trades, all.x=TRUE)
fpt.f <- fpt.f[!is.na(tl_id)]

tl.assoc <- fpt.f[,list(.N), by=list(user_id,tl_id,tl_baid)]

u.assocs <- tl.assoc[,list(tls = length(unique(tl_id))),by=user_id]
t.assocs <- tl.assoc[,list(us = length(unique(user_id))), by=tl_id]

# export trade leader ids
setwd('~/Data/forex')
saveRDS(tl.assoc[,list(tl.user=1),by=tl_id],
        'Rds/trade-leader-users.Rds')
saveRDS(tl.assoc[,list(tl.ba=1),by=tl_baid],
        'Rds/trade-leader-brokeraccounts.Rds')


