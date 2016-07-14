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
    DATA.DIR <- '/archive/gsb/vashevko/forex/'
    DATA.DIR <- '~/Data/forex/'
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
    DATA.DIR <- '~/Data/forex/'
    OUT.DIR <- '~/Dropbox/forex Project/writing/'
    CODE.DIR <- '~/Dropbox/forex Project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)

ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

print(system.time(
        all.adopts <- as.data.table(as.data.frame(ad.ffd[ffwhich(ad.ffd, adopt_uc.grp_select <= 10000),]))
    ))

# drop observations after adoption
all.adopts <- all.adopts[cum.adopt==0]

all.adopts[, oddball25 := rank > 25]
all.adopts[, oddball30 := rank > 30]
all.adopts[, oddball40 := rank > 40]
all.adopts[, oddball50 := rank > 50]





################################################################################
################################################################################
# ANALYSIS TIME
################################################################################
################################################################################

################################################################################
# BASELINE
print(system.time(basem1 <- clogit(badopt ~ ntgt0.a14 + strata(uc.grp), data = all.adopts)))
print(summary(basem1))

print(system.time(basem2 <- clogit(badopt ~ ntgt0.a14 + ndpos.a14 + strata(uc.grp), data = all.adopts)))
print(summary(basem2))

print(system.time(basem3 <- clogit(badopt ~ ntgt0.a14 + ndpos.bc.a14 + strata(uc.grp), data = all.adopts)))
print(summary(basem3))


################################################################################
# RANK SPECIFICATIONS

print(system.time(basem4 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*rank + strata(uc.grp), data = all.adopts)))
print(summary(basem4))

print(system.time(basem4.l <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*log(rank) + strata(uc.grp), data = all.adopts)))
print(summary(basem4.l))



################################################################################
# ODDBALLS

print(system.time(basem5.5 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball5 + strata(uc.grp), data = all.adopts)))
print(summary(basem5.5))

print(system.time(basem5.10 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball10 + strata(uc.grp), data = all.adopts)))
print(summary(basem5.10))

print(system.time(basem5.20 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball20 + strata(uc.grp), data = all.adopts)))
print(summary(basem5.20))

print(system.time(basem5.30 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*oddball30 + strata(uc.grp), data = all.adopts)))
print(summary(basem5.30))


################################################################################
# DOUBLE ODDBALLS
print(system.time(basem6 <- clogit(badopt ~ (ntgt0.a14 + ndpos.a14)*(oddball5 + oddball25) + strata(uc.grp), data = all.adopts)))
print(summary(basem6))


################################################################################
# SAVE MODELS
save.image(file='adopt-analysis-ucfe.Rdata')

