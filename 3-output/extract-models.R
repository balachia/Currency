library(survival)
library(texreg)

setwd('~/Data/forex')

############################################################
# user-fe models

load('Rdata/adopt-analysis-sudocox-user.Rdata')

endings <- c('.base','','.l','.5','.10','.20')
models <- paste0('basem10',endings)
res <- lapply(models, function(x) extract(get(x)))
names(res) <- models

saveRDS(res,file='Rdata/texreg/adopt-analysis-sudocox-user.Rds')

rm(list=ls())
gc()

############################################################
# cp-fe models

load('Rdata/adopt-analysis-sudocox-cp.Rdata')

endings <- c('.base','','.l','.5','.10','.20')
models <- paste0('basem10',endings)
res <- lapply(models, function(x) extract(get(x)))
names(res) <- models

saveRDS(res,file='Rdata/texreg/adopt-analysis-sudocox-cp.Rds')

rm(list=ls())
gc()

############################################################
# cp-fe models

load('Rdata/adopt-analysis-sudocox-cp-friends.Rdata')

endings <- c('.base','','.l','.5','.10','.20')
models <- paste0('basem10',endings)
res <- lapply(models, function(x) extract(get(x)))
names(res) <- models

saveRDS(res,file='Rdata/texreg/adopt-analysis-sudocox-cp-friends.Rds')

rm(list=ls())
gc()



