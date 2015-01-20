# are traders profitable?
library(data.table)
library(plm)
library(lme4)
library(texreg)

rm(list=ls())
options(max.print=1000)

setwd('~/Data/forex/')

fpt <- readRDS('Rds/forexposition.Rds')
dbap <- readRDS('Rds/dailybrokeraccount.Rds')

# overall user profits?
# well that was a stupid exercise:
#   fpt only includes non trade leader profits
#   shows large disagreement with dbap
#   dbap is probably the more accurate reflection of user position
#   but fpt is probably the more accurate reflection of users own trades
fpt.profit <- fpt[,list(fpt.profit=sum(dollarpnl)),by=user_id]
dbap.profit <- dbap[,list(dbap.profit=sum(dollarPnl)),by=user_id]

profits <- merge(fpt.profit,dbap.profit,by='user_id',all=TRUE)

month(fpt[,as.POSIXct(closedate/1000,origin='1970-01-01',tz='UTC')])

fpt[,closect := as.POSIXct(closedate/1000,origin='1970-01-01',tz='UTC')]
fpt[,monthid := year(closect) * 12 + month(closect)]
fpt[,monthid2 := year(closect) * 100 + month(closect)]

# how should we actually calculate profits?
# what fraction of volume traded was lost/won?
# i.e. convert to volume weighted pip pnl
fpt[,currencypnl := volume * (longtr * clopdiff) / openprice]
fpt[,cp := paste0(currency1,currency2)]

setkey(fpt,user_id,cp,monthid)

# n.b. for reporting purposes, i'm normalizing profit to be a percentage
user.profits <- fpt[,list(monthid2=min(monthid2),
                          .N,
                          currencypnl=sum(currencypnl),
                          volume=sum(as.numeric(volume)),
                          profit=100 * sum(currencypnl) / sum(as.numeric(volume))),
                    by=list(user_id,cp,monthid)]
user.profits[,profit.l1 := c(NA,head(profit,-1)), by=list(user_id,cp)]
user.profits[order(profit.l1), profitrank:= (1:.N)/.N, by=list(cp,monthid)]
user.profits[,profitrankcut := cut(profitrank,c(0,0.01,0.05,0.2,0.4,0.6,0.8,0.95,0.99,1))]
user.profits[,rand := runif(.N)]

cps <- user.profits[,.N,by=cp][order(-N)][1:5,cp]

res <- lapply(cps, function(ccp) {
    cat(ccp,'\n')
    up.cp <- user.profits[cp==ccp]

    # linear models
    acm.lag <- lm(profit ~ profit.l1,
                  data=up.cp)
    acm.rank <- lm(profit ~ profitrank,
                   data=up.cp)
    acm.rank2 <- lm(profit ~ profitrankcut - 1,
                    data=up.cp)

    poolm <- plm(profit ~ 1, data=up.cp,
                 model='pooling',index=c('user_id','monthid'))
    rem <- lmer(profit ~ 1 + (1|user_id),
                data=up.cp)

    list(lag=acm.lag,
         rank=acm.rank,
         rank2=acm.rank2,
         poolm=poolm,
         rem=rem)
})

# extract models
lags <- lapply(res, function(x) x$lag)
ranks <- lapply(res, function(x) x$rank)
rank2s <- lapply(res, function(x) x$rank2)
poolms <- lapply(res, function(x) x$poolm)
rems <- lapply(res, function(x) x$rem)

names(lags) <- cps
names(ranks) <- cps
names(rank2s) <- cps
names(poolms) <- cps
names(rems) <- cps

screenreg(lags, digits=4)
screenreg(ranks, digits=4)
screenreg(rank2s, digits=4)

bp.pvals <- sapply(poolms, function(x) {
    plmtest(x,type='bp',effect='individual')$p.value
})

sapply(rems, function(x) fivenum(coef(x)$user_id[,1]))
sapply(rems, function(x) coef(summary(x)))

sapply(rems, function(x) quantile(coef(x)$user_id[,1],c(0,0.01,seq(0.05,0.95,0.05),0.99,1)))

saveRDS(list(lags=lags,
             ranks=ranks,
             rank2s=rank2s,
             poolms=poolms,
             rems=rems),
        'Rds/trader-skill-test-results.Rds')

#up.eurusd <- user.profits[cp=='EURUSD']

#acm.lag <- lm(profit ~ profit.l1,
              #data=up.eurusd)
#acm.rank <- lm(profit ~ profitrank,
               #data=up.eurusd)
#acm.rank2 <- lm(profit ~ profitrankcut,
                #data=up.eurusd)
#summary(acm.lag)
#summary(acm.rank)
#summary(acm.rank2)

#poolm <- plm(profit ~ 1, data=up.eurusd,
             #model='pooling',index=c('user_id','monthid'))
#rem <- lmer(profit ~ 1 + (1|user_id),
            #data=up.eurusd)
#summary(poolm)
#summary(rem)
#plmtest(poolm,type='bp',effect='individual')
#summary(coef(rem)$user_id)
#quantile(coef(rem)$user_id[,1],c(0,0.01,seq(0.05,0.95,0.05),0.99,1))

# weird replication of osler2003
# tp last digits:
tp.last <- fpt[!is.na(takeProfit), round((10000 * takeProfit) %% 100)]
txtdensity(tp.last)

# stop loss last digits:
sl.last <- fpt[!is.na(stopLoss), round((10000 * stopLoss) %% 100)]
txtdensity(sl.last)

sltp.diffs <- table(sl.last) / length(sl.last) - table(tp.last) / length(tp.last)
txtplot(0:100,sltp.diffs)


