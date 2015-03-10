library(survival)
library(texreg)
library(stargazer)

# source utilities
source('table-utilities.R')

setwd('~/Data/forex')
#load('Rdata/adopt-analysis-sudocox-user.Rdata')
#load('Rdata/adopt-analysis-sudocox-cp.Rdata')
#load('Rdata/adopt-analysis-sudocox-cp-friends.Rdata')

userms <- readRDS('Rdata/texreg/adopt-analysis-sudocox-user.Rds')
cpms <- readRDS('Rdata/texreg/adopt-analysis-sudocox-cp.Rds')
cpfrms <- readRDS('Rdata/texreg/adopt-analysis-sudocox-cp-friends.Rds')

setwd('~/2YP/writing/tables/')

addBelowTex <- function(x,rows=NULL) {
    addRows <- ''
    for(i in seq_along(rows)) {
        addRows <- paste0(addRows,paste(rows[[i]],collapse=' & '),' \\\\\\\\ \n')
    }
    repstr <- paste0('\\1\\midrule\\2\\midrule \n', addRows)
    gsub('(.*)\\midrule(.*)\\midrule',repstr,x)
}

# shit needs work
# basically insert rows above the border-top rows
# make the top-most row of things have border-top style
# make the previous border-top have regular style
addBelowHTML <- function(x,rows=NULL) {
    styletop <- '"border-top: 1px solid black;"'
    stylemid <- '"padding-right: 12px; border: none;"'
    stylerep <- '"STYLE_REP_TOP"'
    addRows <- ''
    for(i in seq_along(rows)) {
        style <- ifelse(i==1,stylerep,stylemid)
        front <- paste0('<td style=',style,'>')
        back <- '</td>'
        join <- paste0(back,'\n',front)
        innards <- paste(rows[[i]],collapse=join)
        addRows <- paste0(addRows,'<tr>\n',front,
                          innards,
                          back,'\n</tr>\n')
    }
    print(addRows)
    repstr <- paste0('\\1',addRows,'<tr>\\2<td style="border-top')
    res <- gsub('(.*)<tr>(\\s*)<td style="border-top',repstr,x)
    res <- gsub(styletop,stylemid,res,fixed=TRUE)
    res <- gsub(stylerep,styletop,res,fixed=TRUE)
    res
}


############################################################
# just user effects

basemodel <- function(f,...) {
    f(l=userms['basem10.base', 'basem10.l', 'basem10.10'],
      label='tab:',
      digits=3, float.pos='htb',
      caption='Conditional Logit: Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names='',
      ...)
}


############################################################
# just cp-friends effects

basemodel <- function(f,...) {
    f(l=list(basem10.base, basem10.l, basem10.10),
      label='tab:',
      digits=3, float.pos='htb',
      caption='Conditional Logit: Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names='',
      ...)
}


############################################################
# both cp effects

basemodel <- function(f,...) {
    f(l=list(basem10.base, basem10.l, basem10.10),
      label='tab:',
      digits=3, float.pos='htb',
      caption='Conditional Logit: Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names='',
      ...)
}


############################################################
# user and cp effects

basemodel <- function(f,...) {
    f(l=list(basem10.base, basem10.l, basem10.10),
      label='tab:',
      digits=3, float.pos='htb',
      caption='Conditional Logit: Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names='',
      ...)
}

















