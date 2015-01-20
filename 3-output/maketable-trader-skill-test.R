library(data.table)
library(plm)
library(lme4)
library(texreg)

rm(list=ls())
options(max.print=1000)

setwd('~/Data/forex')

res <- readRDS('Rds/trader-skill-test-results.Rds')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

screenreg(res$lags, digits=4)
screenreg(res$ranks, digits=4)
screenreg(res$rank2s, digits=4)
screenreg(res$poolms, digits=4)
screenreg(res$rems, digits=4)

################################################################################

texreg(res$lags,
       file='trade-skill-lags.tex', label='tab:skill-lags',
       digits=4, float.pos='htb',
       caption='Predictability of Trader Profits: Month-to-Month Returns',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       #custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=2:1,
       custom.coef.names=c('(Intercept)','Prior Month Return'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(res$lags,
       doctype=FALSE, star.symbol='&lowast;',
       digits=4, float.pos='htb',
       caption='Predictability of Trader Profits: Month-to-Month Returns',
       #custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=2:1,
       custom.coef.names=c('(Intercept)','Prior Month Return'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='trade-skill-lags.html')

################################################################################

texreg(res$rank2s,
       file='trade-skill-cuts.tex', label='tab:skill-cuts',
       digits=4, float.pos='htb',
       caption='Predictability of Trader Profits: Quantile of Prior Returns',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       #custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('%ile: 0-1','%ile: 1-5','%ile: 5-20',
                           '%ile: 20-40','%ile: 40-60','%ile: 60-80',
                           '%ile: 80-95','%ile: 95-99','%ile: 99-100'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(res$rank2s,
       doctype=FALSE, star.symbol='&lowast;',
       digits=4, float.pos='htb',
       caption='Predictability of Trader Profits: Quantile of Prior Returns',
       #custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('%ile: 0-1','%ile: 1-5','%ile: 5-20',
                           '%ile: 20-40','%ile: 40-60','%ile: 60-80',
                           '%ile: 80-95','%ile: 95-99','%ile: 99-100'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='trade-skill-cuts.html')

