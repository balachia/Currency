library(survival)
library(texreg)

setwd('~/Data/forex')
#load('adopt-analysis.Rdata')
load('Rdata/adopt-analysis-basem.Rdata')
load('Rdata/adopt-analysis-oddball.Rdata')
load('Rdata/adopt-analysis-rank.Rdata')

load('Rdata/adopt-analysis-sudocox-user.Rdata')
load('Rdata/adopt-analysis-sudocox-cp.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################
# FULL MODELS
################################################################################

texreg(list(basem2, basem4.l, basem5.5, basem5.10, basem5.20),
       file='adopts-base-log-dummies.tex', label='tab:adopts-bld',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
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
       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log-dummies.html')

############################################################
# sudocox user

texreg(list(basem10.base, basem10, basem10.l, basem10.5, basem10.10, basem10.20),
       file='adopts-full-sudocox-user.tex', label='tab:adopts-sudocox-user',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Pair Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank', '(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem10.base, basem10, basem10.l, basem10.5, basem10.10, basem10.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Pair Adoption',
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank', '(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-full-sudocox-user.html')

############################################################
# sudocox cp

texreg(list(basem11.base, basem11, basem11.l, basem11.5, basem11.10, basem11.20),
       file='adopts-full-sudocox-cp.tex', label='tab:adopts-sudocox-cp',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Pair Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank', '(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem11.base, basem11, basem11.l, basem11.5, basem11.10, basem11.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Pair Adoption',
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank', '(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-full-sudocox-cp.html')


################################################################################
# BASELINE MODELS
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

texreg(list(basem10.base, basem10, basem10.l),
       file='adopts-base-log-sudocox.tex', label='tab:adopts-bl-sudocox',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem10.base, basem10, basem10.l),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log-sudocox.html') 

################################################################################

texreg(list(basem11.base, basem11, basem11.l),
       file='adopts-base-log-sudocox-cp.tex', label='tab:adopts-bl-sudocox-cp',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem11.base, basem11, basem11.l),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption',
       custom.model.names=c('(1) Baseline', '(2) Linear Rank', '(3) Log Rank'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log-sudocox-cp.html') 

################################################################################
# ODDBALL MODELS
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

hstr <- htmlreg(list(basem5.5, basem5.10, basem5.20),
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

################################################################################

texreg(list(basem10.5, basem10.10, basem10.20),
       file='adopts-dummies-sudocox.tex', label='tab:adopts-dm-sudocox',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Adoption (Binary Thresholds)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem10.5, basem10.10, basem10.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Trader/Day): Chance of Currency Adoption (Binary Thresholds)',
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-dummies-sudocox.html') 

################################################################################

texreg(list(basem11.5, basem11.10, basem11.20),
       file='adopts-dummies-sudocox-cp.tex', label='tab:adopts-dm-sudocox-cp',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption (Binary Thresholds)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem11.5, basem11.10, basem11.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=3, float.pos='tb',
       caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption (Binary Thresholds)',
       custom.model.names=c('(4) Rank > 5', '(5) Rank > 10', '(6) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank', 'Trade*Rank', 'Profit*Rank'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-dummies-sudocox-cp.html') 


################################################################################

