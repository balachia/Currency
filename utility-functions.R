library(data.table)



qcuts <- function(x, qs) {
    cut(x, unique(quantile(x, qs, na.rm=TRUE)), labels=FALSE, include.lowest=TRUE)
}

poor.cem <- function(dt, keys, snames=NULL, qnames=NULL, bkeys=keys) {
    kdt <- dt[,c(keys,snames),with=FALSE]
    setkeyv(dt,keys)
    setkeyv(kdt,keys)
    
    print(kdt)
#     print(names(dt))
    
    cat('making quantiles\n')
    lapply(qnames, function (x) {
        cat('processing ',x,'\n')
        kdt[,paste0(x,'_q') := qcuts(dt[,get(x)], seq(0,1,0.1))]
        NULL
    })
    
    allnames <- c(snames, paste0(qnames,'_q'))
    
    cat('making groups\n')
    kdt[,grp := .GRP, by=allnames]
    
    ngrps <- kdt[,max(grp)]
    cat('# Groups:', ngrps, '\n')

    cat('balance statistics\n')
    cat('# observations\n')
    dat <- kdt[,.N,by=grp][,N]
    print(summary(dat))
    print(quantile(dat,seq(0,1,0.1), na.rm=TRUE))
    cat('\n')
    
    for(key in bkeys) {
        cat(key,':\n')
#        dat <- kdt[,dim(.SD[,.N,by=key])[1],by=grp][,V1]
        dat <- kdt[,length(unique(get(key))),by=grp][,V1]

        # try a parallel version...
#        dats <- mclapply(split(1:ngrps, factor(1:ngrps %% (64 * 2))), 
#                         mc.cores=64, mc.preschedule=FALSE,
#                         function (subgrps) {
#                             sub.dat <- kdt[grp %in% subgrps, length(unique(key)), by=grp][,V1]
#                             sub.dat
#                         })
#        dat <- rbindlist(dats)
#        print(dats)
#        print(dat)

        print(summary(dat))
        print(quantile(dat,seq(0,1,0.1), na.rm=TRUE))
        cat('# == 1 ::', sum(dat==1), '(', sum(dat==1) / length(dat), '%)\n')
        cat('\n')
    }
    
    kdt[,c(keys,'grp'), with=FALSE]
}



build.lag <- function(dt, lag, collapseby, affix = '') {
    out <- rbindlist(lapply(0:13, function (x) {
            res <- data.table(dt)
            res[, day := day + x]
            res
        }))
    out <- out[, list(ntotal = sum(ntotal),
                      npos = sum(npos),
                      nneg = sum(nneg)
                      ), by=collapseby]
    setnames(out, c('ntotal','npos','nneg'),
             paste0(c('ntotal','npos','nneg'), affix))

    out
}
