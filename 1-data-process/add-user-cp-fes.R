library(ffbase)
library(stringr)
library(parallel)
library(data.table)



# MATCH FILE SETTINGS TO HOST MACHINE
MC.CORES <- 2
if (grepl('.*stanford\\.edu',Sys.info()[['nodename']])) {
    DATA.DIR <- '/archive/gsb/vashevko/forex/'
    OUT.DIR <- '~/2YP/writing/'
    CODE.DIR <- '~/2YP/code/'
    
    # yen match
    yenre <- str_match(Sys.info()[['nodename']], 'yen(\\d)\\.stanford\\.edu')
    if (yenre[1,2] == '5') {
        MC.CORES <- 60
    } else if (yenre[1,2] == '6' | yenre[1,2] == '7') {
        MC.CORES <- 20
#        MC.CORES <- 6
    }
} else {
    DATA.DIR <- '~/Data/forex/'
    OUT.DIR <- '~/Dropbox/forex Project/writing/'
    CODE.DIR <- '~/Dropbox/forex Project/code/'
}

# LOAD UTILITY FUNCTIONS
source(paste0(CODE.DIR,'utility-functions.R'))

setwd(DATA.DIR)

# load data
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd
open(ad.ffd)


q.names <- NULL
s.names <- c('user_id','cp')

all.adopts <- as.data.table(as.data.frame( ad.ffd[, unique(c('badopt', 'id', q.names, s.names))] ))

ptm <- proc.time()
grps <- poor.cem(all.adopts,
                 keys=c('id'),
                 snames=s.names,
                 qnames=q.names,
                 bkeys=c('badopt'))
print(proc.time() - ptm)

gc()


# MATCHING POST-PROCESSING
# finally, add in groups and shit
grp.ffd <- as.ffdf(grps)
idx.x <- ffdforder(ad.ffd['id'])
idx.y <- ffdforder(grp.ffd['id'])

ad.ffd$uc.grp <- ff(0, nrow(ad.ffd), vmode='integer')
ad.ffd$uc.grpN <- ff(0, nrow(ad.ffd), vmode='integer')

ad.ffd[idx.x, 'uc.grp'] <- grp.ffd[idx.y, 'grp']
ad.ffd[idx.x, 'uc.grpN'] <- grp.ffd[idx.y, 'grpN']

save.ffdf(ad.ffd, dir='./ffdb/all-adopts/', overwrite=TRUE)
close(ad.ffd)
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

rm(grps)
gc()

# FIND VALID GROUPS
# where valid = groups that have both failures and events

ptm <- proc.time()
max.grp <- nrow(ad.ffd)
gap <- 1e6
grp.seq <- seq(1, max.grp, gap)
res <- mclapply(grp.seq,
                mc.cores=MC.CORES, mc.preschedule=FALSE,
                function (x) {
                    cat(x / gap,'/',max.grp / gap,'\n')
                    print(system.time(
                                      dt <- as.data.table(as.data.frame(ad.ffd[ff(x:(x+gap-1)),]))
                                      ))
                    dt[, list(has0 = 0 %in% badopt,
                              has1 = 1 %in% badopt,
                              has0.2 = 0 %in% badopt2,
                              has1.2 = 1 %in% badopt2), by=grp]
                })
res <- rbindlist(res)
hasboth.dt <- res[, list(hasboth = any(has0) & any(has1),
                         hasboth2 = any(has0.2) & any(has1.2)), by=grp]
print(proc.time() - ptm)

# group selector
set.seed(1)
hasboth.dt[, rand_selector := runif(.N)]
hasboth.dt <- hasboth.dt[order(rand_selector)]
hasboth.dt[(!hasboth), adopt_grp_select := Inf]
hasboth.dt[(hasboth), adopt_grp_select := 1:.N]
hasboth.dt[(!hasboth2), adopt2_grp_select := Inf]
hasboth.dt[(hasboth2), adopt2_grp_select := 1:.N]

grps <- data.table(grp=ad.ffd[,'grp'])
grps <- merge(grps, hasboth.dt, by='grp')

options(fftempdir = './tmp')

# add in the group types, randomization
ad.ffd$uc.hasboth <- ff(grps[,hasboth])
ad.ffd$uc.hasboth2 <- ff(grps[,hasboth2])
ad.ffd$uc.rand_selector <- ff(grps[,rand_selector])
ad.ffd$adopt_uc.grp_select <- ff(grps[,adopt_grp_select])
ad.ffd$adopt2_uc.grp_select <- ff(grps[,adopt2_grp_select])

save.ffdf(ad.ffd, dir='./ffdb/all-adopts/', overwrite=TRUE)
close(ad.ffd)
ad.ffd <- load.ffdf('./ffdb/all-adopts/')$ad.ffd

