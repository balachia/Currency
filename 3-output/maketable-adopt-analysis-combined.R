library(survival)
library(texreg)
library(stargazer)

# source utilities
setwd('~/2YP/code')
source('3-output/table-utilities.R')

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
    #print(addRows)
    repstr <- paste0('\\1',addRows,'<tr>\\2<td style="border-top')
    res <- gsub('(.*)<tr>(\\s*)<td style="border-top',repstr,x)
    res <- gsub(styletop,stylemid,res,fixed=TRUE)
    res <- gsub(stylerep,styletop,res,fixed=TRUE)
    res
}


############################################################
# just user effects

wrap <- function(f,...) {
    f(l=userms[c('basem10.base','basem10.l','basem10.10')],
      label='tab:adopts-short-user',
      digits=3, float.pos='tb',
      caption='Conditional Logit (Trader/Day): Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names=c('Alter Trade','Alter Profit',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank','Trade*Rank','Profit*Rank'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)
htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-short-sudocox-user.tex')
write(htmlstr, file='adopts-short-sudocox-user.html')


############################################################
# just cp-friends effects

wrap <- function(f,...) {
    f(l=cpfrms[c('basem11b.base','basem11b.l','basem11b.10')],
      label='tab:adopts-short-cp-friends',
      digits=3, float.pos='tb',
      caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(4) Baseline','(5) Log Rank','(6) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names=c('Alter Trade','Alter Profit',
                          'Friends','Trade*Friends','Profit*Friends',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)
htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-short-sudocox-cp-friends.tex')
write(htmlstr, file='adopts-short-sudocox-cp-friends.html')


############################################################
# both cp effects

wrap <- function(f,...) {
    f(l=c(cpms[c('basem11.base','basem11.l','basem11.10')],cpfrms[c('basem11b.base','basem11b.l','basem11b.10')]),
      label='tab:adopts-short-cp-all',
      digits=3, float.pos='tb',sideways=TRUE,
      caption='Conditional Logit (Currency Pair/Day): Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(4) Baseline','(5) Log Rank','(6) Rank > 10', '(7) Baseline', '(8) Log Rank', '(9) Rank > 10'),
      reorder.coef=NULL,
      omit.coef='^Rank$',
      custom.coef.names=c('Alter Trade','Alter Profit',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Friends','Trade*Friends','Profit*Friends',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)

#rows <- list(c('Trader FE',rep(c('N','N'),each=3)),
#             c('Currency Pair FE',rep(c('N','Y'),each=3)))
#addBelowTex(texstr,rows)
#addBelowHTML(htmlstr,rows)

htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-short-sudocox-cp-all.tex')
write(htmlstr, file='adopts-short-sudocox-cp-all.html')


############################################################
# user and cp effects

wrap <- function(f,...) {
    f(l=c(userms[c('basem10.base','basem10.l','basem10.10')],cpfrms[c('basem11b.base','basem11b.l','basem11b.10')]),
      label='tab:adopts-short-all',
      digits=3, float.pos='tb',sideways=TRUE,
      caption='Conditional Logit: Chance of Currency Adoption',
      dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
      doctype=FALSE, star.symbol='&lowast;',
      include.rsquared=FALSE, include.maxrs=FALSE, include.missings=FALSE,
      custom.model.names=c('(1) Baseline','(2) Log Rank','(3) Rank > 10', '(4) Baseline', '(5) Log Rank', '(6) Rank > 10'),
      reorder.coef=NULL,
      custom.coef.names=c('Alter Trade','Alter Profit',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Rank','Trade*Rank','Profit*Rank',
                          'Friends','Trade*Friends','Profit*Friends',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends',
                          'Rank*Friends','Trade*Rank*Friends','Profit*Rank*Friends'),
      ...)
}

texstr <- wrap(texreg)
htmlstr <- wrap(htmlreg)

rows <- list(c('Trader FE',rep(c('Y','N'),each=3)),
             c('Currency Pair FE',rep(c('N','Y'),each=3)))
texstr <- addBelowTex(texstr,rows)
htmlstr <- addBelowHTML(htmlstr,rows)

htmlstr <- strip.html(htmlstr)

write(texstr, file='adopts-short-sudocox-all.tex')
write(htmlstr, file='adopts-short-sudocox-all.html')




