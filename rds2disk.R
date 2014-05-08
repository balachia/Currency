library(data.table)
library(ffbase)

rm(list=ls())

setwd('/archive/gsb/vashevko/Currensee/')

files <- paste0('Rds/success.by.currency-', c('0-0.2', '0.2-0.4',
                                              '0.4-0.6', '0.6-0.8',
                                              '0.8-1'), 'samp.Rds')

ffd <- NULL

for (file in files) {
  print(file)
  
  c.dt <- readRDS(file)
  c.dt[,cp := as.factor(cp)]
  c.dt[,type := as.factor(type)]

  if(is.null(ffd)) {
    ffd <- as.ffdf(c.dt)
  } else {
    ffd <- ffdfappend(ffd,c.dt)
  }
  
  rm(c.dt)
  gc()
}

save.ffdf(ffd,dir='./ffdb/sbc/')
close(ffd)
