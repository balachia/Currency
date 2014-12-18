# no idea what the point of this file was

library(data.table)

per.cps <- readRDS('Rds/per-cp-results.m4.Rds')

coefs1 <- lapply(per.cp, function (x) {x$cph[[1]]$coefficients})
ses1 <- lapply(per.cp, function (x) {sqrt(diag(x$cph[[1]]$coefficients})))

len <- length(coefs1)

len <- length(per.cps)

all.res <- lapply(1:9, function(regi) {
    coefs <- lapply(per.cp, function (x) { x$cph[[regi]]$coefficients })
    ses <- lapply(per.cp, function (x) { sqrt(diag(x$cph[[regi]]$var ))})
    
    res <- lapply(1:len, function(i) {
        out <- coefs[[i]] / ses[[i]]
        out
    })
    res
})

any.signif <- lapply(1:9, function (i) {
    cat('reg',i,'\n')
    res <- lapply(1:len, function(j) {
        print(j)
        (any(abs(all.res[[i]][[j]]) > 1.96))
    })
    which(res==TRUE)
})

good.by.cp <- lapply(1:len, function(i) {
    out <- list()
    out$name <- per.cps[[i]]$cp

    errs <- lapply(1:9, function (j) {
        'coxph' %in% class(per.cps[[i]]$cph[[j]])
    })

    out$errs <- unlist(errs)
    out$errct <- sum(out$errs)
    out
})

