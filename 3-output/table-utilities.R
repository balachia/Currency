
# strips leading spaces from html, else markdown gets fucked up
strip.html <- function(x) gsub("\\n\\s+","\n",x)

# add gof shit to a table
addGOF <- function(x, ...) {
    gof.names <- c(x@gof.names,"test")
    gof <- c(x@gof,"a")
    gof.decimal <- c(x@gof.decimal,FALSE)

    createTexreg(coef.names = x@coef.names,
                 coef = x@coef,
                 se = x@se,
                 pvalues = x@pvalues,
                 ci.low = x@ci.low,
                 ci.up = x@ci.up,
                 gof.names= gof.names,
                 gof = gof,
                 gof.decimal = gof.decimal,
                 model.name = x@model.name)
}

addBelowTex <- function(x,rows=NULL) {
    addRows <- ''
    for(i in seq_along(rows)) {
        addRows <- paste0(addRows,paste(rows[[i]],collapse=' & '),' \\\\\\\\ \n')
    }
    repstr <- paste0('\\1\\midrule\\2\\midrule \n', addRows)
    gsub('(.*)\\midrule(.*)\\midrule',repstr,x)
}

# shit needs work
# basically insert rows above the border-top rows
# make the top-most row of things have border-top style
# make the previous border-top have regular style
addBelowHTML <- function(x,rows=NULL) {
    styletop <- '"border-top: 1px solid black;"'
    stylemid <- '"padding-right: 12px; border: none;"'
    stylerep <- '"STYLE_REP_TOP"'
    addRows <- ''
    for(i in seq_along(rows)) {
        style <- ifelse(i==1,stylerep,stylemid)
        front <- paste0('<td style=',style,'>')
        back <- '</td>'
        join <- paste0(back,'\n',front)
        innards <- paste(rows[[i]],collapse=join)
        addRows <- paste0(addRows,'<tr>\n',front,
                          innards,
                          back,'\n</tr>\n')
    }
    print(addRows)
    repstr <- paste0('\\1',addRows,'<tr>\\2<td style="border-top')
    res <- gsub('(.*)<tr>(\\s*)<td style="border-top',repstr,x)
    res <- gsub(styletop,stylemid,res,fixed=TRUE)
    res <- gsub(stylerep,styletop,res,fixed=TRUE)
    res
}



