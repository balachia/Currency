library(data.table)
library(ffbase)
library(texreg)
library(survival)
library(brglm)
library(elrm)
library(stringr)



#rm(list=ls())

# MATCH FILE SETTINGS TO HOST MACHINE
MC.CORES <- 2
if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/Currensee/'
    DATA.DIR <- '~/Data/Currensee/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
    
    # yen match
    yenre <- str_match(Sys.info()[['nodename']], 'yen(\\d)\\.stanford\\.edu')
    if (yenre[1,2] == '5') {
        MC.CORES <- 60
    } else if (yenre[1,2] == '6' | yenre[1,2] == '7') {
        MC.CORES <- 12
    }
} else {
    DATA.DIR <- '~/Data/Currensee/'
    OUT.DIR <- '~/Dropbox/Currensee Project/writing/'
    CODE.DIR <- '~/Dropbox/Currensee Project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)

ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

print(system.time(
        all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_grp_select <= 5000),]))
    ))

# drop observations after adoption
all.adopts <- all.adopts[cum.adopt==0]

all.adopts[, oddball25 := rank > 25]
all.adopts[, oddball30 := rank > 30]
all.adopts[, oddball40 := rank > 40]
all.adopts[, oddball50 := rank > 50]

#all.adopts.full <- readRDS('Rds/weekly-all-adopts.Rds')
#all.adopts2 <- readRDS('Rds/weekly-short-adopts.Rds')
# all.adopts2 <- readRDS('Rds/weekly-short-adopts.old.Rds')

# add in alter community benchmarking...
# all.adopts2[, ntri.bc.a14 := sign((npos.a14 / ntotal.a14) - all.winfrac)]
# all.adopts2[ntotal.a14 == 0, ntri.bc.a14 := 0]

# we've got some annoying collinearity here:
# all.adopts2[, ndpos.a14 := as.numeric(ntri.a14 == 1)]
# all.adopts2[, ndpos.bc.a14 := as.numeric(ntri.bc.a14 == 1)]

#all.adopts.full[, ntri.a14 := as.factor(ntri.a14)]
# all.adopts2[, ntri.a14 := as.factor(ntri.a14)]
# all.adopts2[, ntri.bc.a14 := as.factor(ntri.bc.a14)]


# add time
# all.adopts2[, year := year(as.Date(day, origin='1970-01-01'))]
# all.adopts2[, month := month(as.Date(day, origin='1970-01-01'))]
# all.adopts2[, week := week(as.Date(day, origin='1970-01-01'))]

# all.adopts2[, tgrp := year*12 + month]


#all.adopts.full.act <- all.adopts.full[user.active == 1]
# all.adopts2.act <- all.adopts2[user.active == 1]

# all.adopts2[, elrm1 := 1]

################################################################################
################################################################################
# ANALYSIS TIME
################################################################################
################################################################################

################################################################################
# BASELINE
print(system.time(basem1 <- clogit(badopt ~ ntgt0.a14 + strata(grp), data = all.adopts)))
print(summary(basem1))

print(system.time(basem2 <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + strata(grp), data = all.adopts)))
print(summary(basem2))

print(system.time(basem3 <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + strata(grp), data = all.adopts)))
print(summary(basem3))


################################################################################
# CNN EFFECT
print(system.time(basem1.f <- clogit(badopt ~ ntgt0.a14 * nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem1.f))

print(system.time(basem2.f <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem2.f))

print(system.time(basem3.f <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + nfr.a14 + strata(grp), data = all.adopts)))
print(summary(basem3.f))

# alt spec - log
print(system.time(basem1.lf <- clogit(badopt ~ ntgt0.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem1.lf))

print(system.time(basem2.lf <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem2.lf))

print(system.time(basem3.lf <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + log(1 + nfr.a14) + strata(grp), data = all.adopts)))
print(summary(basem3.lf))

# alt spec - poly
print(system.time(basem1.pf <- clogit(badopt ~ ntgt0.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem1.pf))

print(system.time(basem2.pf <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem2.pf))

print(system.time(basem3.pf <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + poly(nfr.a14, degree=2) + strata(grp), data = all.adopts)))
print(summary(basem3.pf))


################################################################################
# RANK SPECIFICATIONS

print(system.time(basem4 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(grp), data = all.adopts)))
print(summary(basem4))

print(system.time(basem4.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.l))

# b-splines
print(system.time(basem4.bs0 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.bs0))

print(system.time(basem4.bs1 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank, knots=10) + strata(grp), data = all.adopts)))
print(summary(basem4.bs1))

print(system.time(basem4.bs2 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*bs(rank, knots=c(5,25)) + strata(grp), data = all.adopts)))
print(summary(basem4.bs2))

# natural splines
print(system.time(basem4.ns0 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank) + strata(grp), data = all.adopts)))
print(summary(basem4.ns0))

print(system.time(basem4.ns1 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank, knots=10) + strata(grp), data = all.adopts)))
print(summary(basem4.ns1))

print(system.time(basem4.ns2 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*ns(rank, knots=c(5,25)) + strata(grp), data = all.adopts)))
print(summary(basem4.ns2))


################################################################################
# ODDBALLS

print(system.time(basem5.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5))

print(system.time(basem5.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10))

print(system.time(basem5.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20))


################################################################################
# ODDBALLS + CNN
print(system.time(basem5.5f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20f <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + nfr.a14)*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))

# log friends
print(system.time(basem5.5lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20lf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + log(1 + nfr.a14))*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))

# poly friends
print(system.time(basem5.5pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball5 + strata(grp), data = all.adopts)))
print(summary(basem5.5f))

print(system.time(basem5.10pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball10 + strata(grp), data = all.adopts)))
print(summary(basem5.10f))

print(system.time(basem5.20pf <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14 + poly(nfr.a14, degree=2))*oddball20 + strata(grp), data = all.adopts)))
print(summary(basem5.20f))


################################################################################
# DOUBLE ODDBALLS
print(system.time(basem6 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*(oddball5 + oddball25) + strata(grp), data = all.adopts)))
print(summary(basem6))


################################################################################
# SAVE MODELS
save.image(file='adopt-analysis.Rdata')

# hard bork
stopifnot(FALSE)

## PLOTS
png('adopt-rank-ecdf.png', width=1600, height=900, pointsize = 12, res=300)
ggplot(all.adopts[badopt==1], aes(rank)) + 
    stat_ecdf() + labs(x=NULL, y=NULL)
dev.off()

# ELRM TEST
# print(system.time(bm1e <- elrm(badopt/elrm1 ~ ntaltGT0 + ntotal.a14 + factor(grp),
#                                interest = ~ ntaltGT0 + ntotal.a14, dataset = all.adopts2)))
# print(summary(bm1e))
# print(system.time(bm1b <- brglm(badopt ~ ntaltGT0 + ntotal.a14 + factor(grp), data = all.adopts2)))
# print(summary(bm1b))

# community bechmark + oddballs
print(system.time(mabrtw1 <- clogit(badopt ~ ntaltGT0*rank + ndpos.bc.a14*rank + strata(grp), data = all.adopts2)))
print(summary(mabrtw1))
print(system.time(mabrtw2 <- clogit(badopt ~ ntaltGT0*poly(rank,2,raw=TRUE) + ndpos.bc.a14*poly(rank,2,raw=TRUE) + strata(grp), data = all.adopts2)))
print(summary(mabrtw2))
print(system.time(mabrtw3 <- clogit(badopt ~ ntaltGT0*oddball10 + ndpos.bc.a14*oddball10 + strata(grp), data = all.adopts2)))
print(summary(mabrtw3))


# other shit...
matw1a <- clogit(badopt ~ ntaltGT0*ntri.e2 + ntri.a14*ntri.e2  + strata(grp), data = all.adopts2)
print(summary(matw1a))
matw2a <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.e2  + strata(grp), data = all.adopts2)
print(summary(matw2a))



# time group stuff
print(system.time(mabtw1tg <- clogit(badopt ~ ntaltGT0 + ndpos.bc.a14  + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabtw1tg))
print(system.time(mabrtw1tg <- clogit(badopt ~ ntaltGT0*rank + ndpos.bc.a14*rank + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw1tg))
print(system.time(mabrtw2tg <- clogit(badopt ~ ntaltGT0*poly(rank,2,raw=TRUE) + ndpos.bc.a14*poly(rank,2,raw=TRUE) + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw2tg))
print(system.time(mabrtw3tg <- clogit(badopt ~ ntaltGT0*oddball10 + ndpos.bc.a14*oddball10 + strata(grp, tgrp), data = all.adopts2)))
print(summary(mabrtw3tg))



# let's test... active users
matw1.act <- clogit(badopt ~ ntaltGT0 + ntri.a14  + strata(grp), data = all.adopts2.act)
print(summary(matw1.act))
matw2.act <- clogit(badopt ~ ntaltGT0*ntri.a14  + strata(grp), data = all.adopts2.act)
print(summary(matw2.act))

matw1a.act <- clogit(badopt ~ ntaltGT0*ntri.e2 + ntri.a14*ntri.e2  + strata(grp), data = all.adopts2.act)
print(summary(matw1a.act))
matw2a.act <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.e2  + strata(grp), data = all.adopts2.act)
print(summary(matw2a.act))



# benchmarking success?
mtw1.be <- clogit(badopt ~ ntaltGT0 + ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(mtw1.be))
mtw2.be <- clogit(badopt ~ ntaltGT0*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(mtw2.be))

matw1a.be <- clogit(badopt ~ ntaltGT0*ntri.be.e2 + ntri.a14*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(matw1a.be))
matw2a.be <- clogit(badopt ~ ntaltGT0*ntri.a14*ntri.be.e2  + strata(grp), data = all.adopts2)
print(summary(matw2a.be))



# re-entry?
all.adopts.full[,badopt.full := ifelse(badopt2 == 1 & uselapse >= 60,1,0)]

matw1f <- clogit(badopt.full ~ ntaltGT0 + ntri.a14  + strata(grp), data = all.adopts.full)
print(summary(matw1f))
matw2f <- clogit(badopt.full ~ ntaltGT0*ntri.a14  + strata(grp), data = all.adopts.full)
print(summary(matw2f))



# write out some results...
setwd(paste0(OUT.DIR,'tables/'))

texreg(list(mabtw1,mabrtw3,mabrtw1,mabrtw2),
       file='adopts-dummies.tex', label='tab:adopt-dummies',
       digits=4, float.pos='htb',
       caption='Conditional Logit on Currency Adoption',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=c('Alter: Any Trades','Alter: > Benchmark',
                           'Oddball Currency','Any Trades:Oddball','> Benchmark:Oddball',
                           'Currency Rank','Any Trades:Rank','> Benchmark:Rank',
                           'Currency Rank','Rank^2','Any Trades:Rank','Any Trades:Rank^2','> Benchmark:Rank','> Benchmark:Rank^2'
                           ))

texreg(list(mtw1,mtw2),
       file='adopts-tw.tex', label='tab:adopt-triwin',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Ego Tri-Wins',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1,matw2),
       file='adopts-atw.tex', label='tab:adopt-atriwin',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Alter Tri-Wins',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1a,matw2a),
       file='adopts-twinter.tex', label='tab:adopt-triwin-inter',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Tri-Win Interactions',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1.act,matw2.act),
       file='adopts-active.tex', label='tab:adopt-active',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Exclude Inactive Users (No activity in 60 days)',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)

texreg(list(matw1.be,matw2.be),
       file='adopts-benchego.tex', label='tab:adopt-benchego',
       digits=5, float.pos='htb',
       caption='Conditional Logit: Benchmark vs All Own Results',
       dcolumn=TRUE, booktabs=TRUE, use.packages=FALSE,
       reorder.coef=NULL,
       custom.coef.names=NULL)
