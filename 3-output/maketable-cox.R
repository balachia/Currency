library(survival)
library(texreg)
library(data.table)

setwd('~/Data/forex')
#load('adopt-analysis.Rdata')
load('Rdata/adopt-analysis-cox-base.Rdata')
load('Rdata/adopt-analysis-cox.Rdata')
load('Rdata/adopt-analysis-cox-user.Rdata')
load('Rdata/adopt-analysis-cox-cp.Rdata')
load('Rdata/adopt-analysis-cox-tlxp.Rdata')
load('Rdata/adopt-analysis-cox-user-tlxp.Rdata')
load('Rdata/adopt-analysis-cox-cp-tlxp.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################

texreg(list(coxm3, coxm3.l, coxm3.5, coxm3.10, coxm3.20),
       file='adopts-cox.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Cox (Single Stratum): Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(coxm3, coxm3.l, coxm3.5, coxm3.10, coxm3.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Cox (Single Stratum): Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-cox.html')


################################################################################

texreg(list(coxm4, coxm4.l, coxm4.5, coxm4.10, coxm4.20),
       file='adopts-cox-user.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Cox (User Strata): Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(coxm4, coxm4.l, coxm4.5, coxm4.10, coxm4.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Cox (User Strata): Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-cox-user.html')


################################################################################

texreg(list(coxm5, coxm5.l, coxm5.5, coxm5.10, coxm5.20),
       file='adopts-cox-cp.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Cox (Currency Pair Strata): Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(coxm5, coxm5.l, coxm5.5, coxm5.10, coxm5.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Cox (Currency Pair Strata): Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.zph=FALSE, include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-cox-cp.html')



