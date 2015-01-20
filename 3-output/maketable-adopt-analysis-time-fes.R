library(survival)
library(texreg)

setwd('~/Data/forex')
load('Rdata/adopt-analysis-time-fes.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################

texreg(list(basem7, basem7.l, basem7.5, basem7.10, basem7.20),
       file='adopts-time-fes.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Chance of Currency Adoption (User/Currency Pair Fixed Effects)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE, sideways=TRUE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem7, basem7.l, basem7.5, basem7.10, basem7.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Chance of Currency Adoption (User/Currency Pair Fixed Effects)',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-time-fes.html')


