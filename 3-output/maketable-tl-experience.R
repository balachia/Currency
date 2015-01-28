library(survival)
library(texreg)

setwd('~/Data/forex')
#load('adopt-analysis.Rdata')
load('Rdata/adopt-analysis-tl.Rdata')
load('Rdata/adopt-analysis-tl-sudocox.Rdata')
load('Rdata/adopt-analysis-tl-sudocox-opens.Rdata')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

################################################################################

texreg(list(basem12, basem12.l, basem12.5, basem12.10, basem12.20),
       file='adopts-tlxp-base.tex', label='tab:adopts-tlxp-base',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Effects of Trade Leader Experience',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                           'Alter Trade*TL','Alter Profit*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem12, basem12.l, basem12.5, basem12.10, basem12.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Effects of Trade Leader Experience',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                           'Alter Trade*TL','Alter Profit*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-tlxp-base.html')

################################################################################

texreg(list(basem12a, basem12a.l, basem12a.5, basem12a.10, basem12a.20),
       file='adopts-tlxp-sudocox.tex', label='tab:adopts-tlxp-sudocox',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Effects of Trade Leader Experience',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                           'Alter Trade*TL','Alter Profit*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

hstr <- htmlreg(list(basem12a, basem12a.l, basem12a.5, basem12a.10, basem12a.20),
       doctype=FALSE, star.symbol='&lowast;',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Effects of Trade Leader Experience',
       custom.model.names=c('(1) Baseline', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
       reorder.coef=NULL,
       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                           'Alter Trade*TL','Alter Profit*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                           'Rank', 'Trade*Rank', 'Profit*Rank',
                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
write(strip.html(hstr), file='adopts-tlxp-sudocox.html')

################################################################################




