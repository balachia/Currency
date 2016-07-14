library(data.table)
library(ffbase)

rm(list=ls())

setwd('/archive/gsb/vashevko/forex/')

cuts <- seq(0,1,0.1)
cut.names <- paste(cuts[-length(cuts)], cuts[-1], sep='-')
files <- paste0('Rds/success.by.currency-', cut.names, 'samp.Rds')

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
