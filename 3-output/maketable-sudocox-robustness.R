library(survival)
library(texreg)

setwd('~/Data/forex')
#load('adopt-analysis.Rdata')
load('Rdata/adopt-analysis-sudocox-cp-friends.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################

texreg(list(basem11b, basem11b.l, basem11b.5, basem11b.10, basem11b.20),
       file='adopts-full-sudocox-cp-friends.tex', label='tab:adopts-sudocox-cp-friends',
       digits=5, float.pos='htb',sideways=TRUE,
       caption='Conditional Logit (Currency Pair/Day): Effects of Peer Group Size',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(7) Rank', '(8) Log Rank', '(9) Rank > 5', '(10) Rank > 10', '(11) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Rank',
                           'Trade*Friends','Profit*Friends',
                           'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem11b, basem11b.l, basem11b.5, basem11b.10, basem11b.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day: Effects of Peer Group Size',
       custom.model.names=c('(7) Rank', '(8) Log Rank', '(9) Rank > 5', '(10) Rank > 10', '(11) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Rank',
                           'Trade*Friends','Profit*Friends',
                           'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-full-sudocox-cp-friends.html')

################################################################################

texreg(list(basem11b.base, basem11b, basem11b.l),
       file='adopts-base-log-sudocox-cp-friends.tex', label='tab:adopts-bl-sudocox-cp-friends',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Effects of Peer Group Size',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(7) Baseline', '(8) Rank', '(9) Log Rank'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Trade*Friends','Profit*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem11b.base, basem11b, basem11b.l),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Effects of Peer Group Size',
       custom.model.names=c('(7) Baseline', '(8) Rank', '(9) Log Rank'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Trade*Friends','Profit*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-base-log-sudocox-cp-friends.html')

################################################################################

texreg(list(basem11b.5, basem11b.10, basem11b.20),
       file='adopts-dummies-sudocox-cp-friends.tex', label='tab:adopts-dm-sudocox-cp-friends',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Effects of Peer Group Size',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(10) Rank > 11', '(11) Rank > 10', '(12) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Rank',
                           'Trade*Friends','Profit*Friends',
                           'Trade*Rank','Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)


hstr <- htmlreg(list(basem11b.5, basem11b.10, basem11b.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit (Currency Pair/Day): Effects of Peer Group Size',
       custom.model.names=c('(10) Rank > 5', '(11) Rank > 10', '(12) Rank > 20'),
       reorder.coef=NULL,
       omit.coef='^Rank$',
       custom.coef.names=c('Alter Trade', 'Alter Profit',
                           'Friends','Rank',
                           'Trade*Friends','Profit*Friends',
                           'Trade*Rank','Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-dummies-sudocox-cp-friends.html')






