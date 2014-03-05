library(data.table)
library(survival)
library(rms)
library(parallel)

setwd('~/Data/Currensee')

rm(list=ls())

addend <- '.m4'
addend <- ''
cores <- 60

#print(system.time(adopt.es.list <- readRDS('Rds/adopt.events.m10.Rds')))
print(system.time(adopt.es.list <- readRDS(paste0('Rds/adopt.events',addend,'.Rds'))))

forms <- c(
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt + npos.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.e10 + npos.alt*npos.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
                    ntotal.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
                    ntotal.e10 + npos.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
                    ntotal.e10*npos.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.e10 + 
                    ntotal.e10*npos.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt*npos.e10 + 
                    ntotal.e10*npos.e10')



res <- mclapply(adopt.es.list, mc.cores=cores, mc.preschedule=FALSE,
    function (cdt) {
        cdt[,ntaltGT0 := as.numeric(ntotal.alt > 0)]

        cphms <- lapply(forms, function (form) {
                cphm <- tryCatch({
                    cph(as.formula(form), data=cdt)}, error=identity)
            })

        #         cph1 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt, data=cdt)}, error= function(e) e)
        #         cph2 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                         poly(ntotal.alt,2,raw=TRUE), data=cdt)}, error= function(e) e)
        #         cph3 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                         poly(ntotal.alt,2,raw=TRUE) + 
        #                         npos.alt, data=cdt)}, error= function(e) e)
        #         cph4 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                         poly(ntotal.alt,2,raw=TRUE) + 
        #                         poly(npos.alt,2,raw=TRUE), data=cdt)}, error= function(e) e)
        #         cph5 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                         ntotal.e10, data=cdt)}, error= function(e) e)
        #         cph6 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                         ntotal.e10 + npos.e10, data=cdt)}, error= function(e) e)
        #         cph7 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                         ntotal.e10*npos.e10, data=cdt)}, error= function(e) e)
        #         cph8 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.e10 + 
        #                         ntotal.e10*npos.e10, data=cdt)}, error= function(e) e)
        #         cph9 <- tryCatch({
        #                     cph(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt*npos.e10 + 
        #                         ntotal.e10*npos.e10, data=cdt)}, error= function(e) e)

        #         psm1 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt, data=cdt)
        #         psm2 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                     poly(ntotal.alt,2,raw=TRUE), data=cdt)
        #         psm3 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                     poly(ntotal.alt,2,raw=TRUE) + 
        #                     npos.alt, data=cdt)
        #         psm4 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + 
        #                     poly(ntotal.alt,2,raw=TRUE) + 
        #                     poly(npos.alt,2,raw=TRUE), data=cdt)
        #         psm5 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                     ntotal.e10, data=cdt)
        #         psm6 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                     ntotal.e10 + npos.e10, data=cdt)
        #         psm7 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt + 
        #                     ntotal.e10*npos.e10, data=cdt)
        #         psm8 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.e10 + 
        #                     ntotal.e10*npos.e10, data=cdt)
        #         psm9 <- psm(Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt*npos.e10 + 
        #                     ntotal.e10*npos.e10, data=cdt)

        res.out <- list()
        res.out$cp <- cdt[1,cp]
        res.out$cph <- cphms
        #         res.out$cph <- list(cph1, cph2, cph3, cph4, cph5, cph6, cph7, cph8, cph9)
        #         res.out$psm <- list(psm1, psm2, psm3, psm4, psm5, psm6, psm7, psm8, psm9)
        
        res.out
    })

#saveRDS(res, 'Rds/per-cp-m10-results.Rds')
saveRDS(res, paste0('Rds/per-cp-results',addend,'.Rds'))

