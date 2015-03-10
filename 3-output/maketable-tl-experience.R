library(survival)
library(texreg)

# source utilities
setwd('~/2YP/code')
source('3-output/table-utilities.R')

setwd('~/Data/forex')
#load('adopt-analysis.Rdata')
#load('Rdata/adopt-analysis-tl.Rdata')
#load('Rdata/adopt-analysis-tl-sudocox-user.Rdata')
#load('Rdata/adopt-analysis-tl-sudocox-opens.Rdata')
#load('Rdata/adopt-analysis-tl-sudocox-cp.Rdata')

userms <- readRDS('Rdata/texreg/adopt-analysis-tl-sudocox-user.Rds')
cpms <- readRDS('Rdata/texreg/adopt-analysis-tl-sudocox-cp.Rds')

setwd('~/2YP/writing/tables/')

strip.html <- function(x) gsub("\\n\\s+","\n",x)

############################################################
# user effects

wrap <- function(f,...) {
    f(l=userms[c('basem12a.base','basem12a.l','basem12a.10')],
      label='tab:adopts-tlxp-sudocox',
      digits=3, float.pos='htb',
      caption='Conditional Logit (Trader/Day): Effects of Trade Leader Experience',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      omit.coef='^Trade Leader$',
      custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                          'Alter Trade*TL','Alter Profit*TL',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                          'Rank', 'Trade*Rank', 'Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)
htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-tlxp-sudocox.tex')
write(htmlstr, file='adopts-tlxp-sudocox.html')


############################################################
# cp effects

wrap <- function(f,...) {
    f(l=cpms[c('basem12c.base','basem12c.l','basem12c.10')],
      label='tab:adopts-tlxp-sudocox-cp',
      digits=3, float.pos='htb',
      caption='Conditional Logit (Currency Pair/Day): Effects of Trade Leader Experience',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      omit.coef='^Rank$',
      custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
                          'Rank','Alter Trade*TL','Alter Profit*TL',
                          'Trade*Rank','Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                          'Rank', 'Trade*Rank', 'Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                          'Rank', 'Trade*Rank', 'Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                          'Rank', 'Trade*Rank', 'Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
                          'Rank', 'Trade*Rank', 'Profit*Rank',
                          'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)
htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-tlxp-sudocox-cp.tex')
write(htmlstr, file='adopts-tlxp-sudocox-cp.html')



















################################################################################

#texreg(list(basem12, basem12.l, basem12.5, basem12.10, basem12.20),
#       file='adopts-tlxp-base.tex', label='tab:adopts-tlxp-base',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit: Effects of Trade Leader Experience',
#       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Alter Trade*TL','Alter Profit*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

#hstr <- htmlreg(list(basem12, basem12.l, basem12.5, basem12.10, basem12.20),
#       doctype=FALSE, star.symbol='&lowast;',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit: Effects of Trade Leader Experience',
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Alter Trade*TL','Alter Profit*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
#write(strip.html(hstr), file='adopts-tlxp-base.html')

################################################################################

#texreg(list(basem12a, basem12a.l, basem12a.5, basem12a.10, basem12a.20),
#       file='adopts-tlxp-sudocox.tex', label='tab:adopts-tlxp-sudocox',
#       digits=5, float.pos='htb',sideways=TRUE,
#       caption='Conditional Logit (Trader/Day): Effects of Trade Leader Experience',
#       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       omit.coef='^Trade Leader$',
#       #omit.coef='^tl.user$',
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Rank','Alter Trade*TL','Alter Profit*TL',
#                           'Trade*Rank','Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

#hstr <- htmlreg(list(basem12a, basem12a.l, basem12a.5, basem12a.10, basem12a.20),
#       doctype=FALSE, star.symbol='&lowast;',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit (Trader/Day): Effects of Trade Leader Experience',
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       omit.coef=c('^Trade Leader$'),
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Rank','Alter Trade*TL','Alter Profit*TL',
#                           'Trade*Rank','Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
#write(strip.html(hstr), file='adopts-tlxp-sudocox.html')

################################################################################

#texreg(list(basem12c, basem12c.l, basem12c.5, basem12c.10, basem12c.20),
#       file='adopts-tlxp-sudocox-cp.tex', label='tab:adopts-tlxp-sudocox-cp',
#       digits=5, float.pos='htb',sideways=TRUE,
#       caption='Conditional Logit (Currency Pair/Day): Effects of Trade Leader Experience',
#       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       omit.coef='^Rank$',
#       #omit.coef='^tl.user$',
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Rank','Alter Trade*TL','Alter Profit*TL',
#                           'Trade*Rank','Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

#hstr <- htmlreg(list(basem12c, basem12c.l, basem12c.5, basem12c.10, basem12c.20),
#       doctype=FALSE, star.symbol='&lowast;',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit (Currency Pair/Day): Effects of Trade Leader Experience',
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       omit.coef=c('^Rank$'),
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Rank','Alter Trade*TL','Alter Profit*TL',
#                           'Trade*Rank','Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
#write(strip.html(hstr), file='adopts-tlxp-sudocox-cp.html')

################################################################################
# these models don't solve

#texreg(list(basem12b, basem12b.l, basem12b.5, basem12b.10, basem12b.20),
#       file='adopts-tlxp-sudocox.tex', label='tab:adopts-tlxp-sudocox',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit: Effects of Trade Leader Experience',
#       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Alter Trade*TL','Alter Profit*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)

#hstr <- htmlreg(list(basem12b, basem12b.l, basem12b.5, basem12b.10, basem12b.20),
#       doctype=FALSE, star.symbol='&lowast;',
#       digits=5, float.pos='htb',
#       caption='Conditional Logit: Effects of Trade Leader Experience',
#       custom.model.names=c('(1) Linear Rank', '(2) Log Rank', '(3) Rank > 5', '(4) Rank > 10', '(5) Rank > 20'),
#       reorder.coef=NULL,
#       custom.coef.names=c('Alter Trade', 'Alter Profit','Trade Leader',
#                           'Alter Trade*TL','Alter Profit*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL',
#                           'Rank', 'Trade*Rank', 'Profit*Rank',
#                           'Rank*TL','Trade*Rank*TL','Profit*Rank*TL'),
#       include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE)
#write(strip.html(hstr), file='adopts-tlxp-sudocox.html')

################################################################################




