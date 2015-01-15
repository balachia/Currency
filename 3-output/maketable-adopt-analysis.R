library(survival)
library(texreg)

setwd('~/Data/forex')
load('adopt-analysis.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################

texreg(list(basem2, basem4.l, basem5.5, basem5.10, basem5.20),
       file='adopts-base-log-dummies.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem2, basem4.l, basem5.5, basem5.10, basem5.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log-dummies.html')

################################################################################

texreg(list(basem2, basem4, basem4.l),
       file='adopts-base-log.tex', label='tab:adopts-bl',
       digits=3, float.pos='tb',
       caption='Conditional Logit: Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem2, basem4, basem4.l),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit: Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log.html') 

################################################################################

texreg(list(basem5.5, basem5.10, basem5.20),
       file='adopts-dummies.tex', label='tab:adopts-dm',
       digits=3, float.pos='tb',
       caption='Conditional Logit: Chance of Currency Adoption (Binary Thresholds)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

htmlreg(list(basem5.5, basem5.10, basem5.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit: Chance of Currency Adoption (Binary Thresholds)',
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-dummies.html') 

