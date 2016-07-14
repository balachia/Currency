library(data.table)
library(survival)
library(rms)
library(parallel)

setwd('~/Data/forex')

rm(list=ls())

addend <- '.m4'
print(system.time(adopt.es.list <- readRDS(paste0('Rds/adopt.events',addend,'.Rds'))))

forms <- c(
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt + npos.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.e10 + npos.alt*npos.e10',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*rank + npos.alt*rank',
        'Surv(start,stop,badopt) ~ ntaltGT0 + ntotal.alt*npos.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + 
                    poly(ntotal.alt,2,raw=TRUE)',
        'Surv(start,stop,badopt) ~ ntaltGT0 + 
                    poly(ntotal.alt,2,raw=TRUE) + 
                    npos.alt',
        'Surv(start,stop,badopt) ~ ntaltGT0 + 
                    poly(ntotal.alt,2,raw=TRUE) + 
                    poly(npos.alt,2,raw=TRUE)',
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

form.names <- paste0('cph',1:9)

adopt.es <- rbindlist(adopt.es.list)
adopt.es[,ntaltGT0 := as.numeric(ntotal.alt > 0)]

res <- mclapply(forms, mc.cores=60, mc.preschedule=FALSE,
    function(form) {
        cph.out <- cph(as.formula(form), data=adopt.es, singular.ok=TRUE)
        cat('done ::', form, '\n')
        cph.out
    })

models.out <- list()
models.out$forms <- forms
models.out$names <- form.names
models.out$models <- res

saveRDS(models.out, paste0('Rds/all-cp-results',addend,'.Rds'))

