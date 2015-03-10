library(survival)
library(data.table)
library(ggplot2)
library(RColorBrewer)

setwd('~/Data/forex')
load('Rdata/adopt-analysis-tl-sudocox-user.Rdata')

rm(basem12a.base,basem12a,basem12a.5,basem12a.10,basem12a.20)
gc()

setwd('~/2YP/writing/plots/')
 
setEPS()

######################################################################

plotdata <- CJ(type=c('none','trade','profit'),
               #ntgt0.a14=c(FALSE,TRUE), ndpos.a14=c(FALSE,TRUE),
               rank=1:133, tl.user=0:1)

# proper parameters
plotdata[type == 'none', `:=`(ntgt0.a14=0, ndpos.a14=0)]
plotdata[type == 'trade', `:=`(ntgt0.a14=1, ndpos.a14=0)]
plotdata[type == 'profit', `:=`(ntgt0.a14=1, ndpos.a14=1)]
plotdata[, rank.l := log(rank)]


# make predictions
coefs <- basem12a.l$coefficients
ses <- sqrt(diag(basem12a.l$var))

# get rid of NAs
coefs[is.na(coefs)] <- 0

# exclude baseline currency pair effect
coefs['log(rank)'] <- 0

vars <- as.matrix(plotdata[,list(ntgt0.a14,ndpos.a14,rank.l,tl.user)])
coef.cols <- list(1,2,4,3,
                  c(1,4),
                  c(2,4),
                  c(1,3),
                  c(2,3),
                  c(3,4),
                  c(1,3,4),
                  c(2,3,4))

res <- rep(0,nrow(vars))
for (i in 1:length(coefs)) {
    res <- res + coefs[i] * apply(as.matrix(vars[,coef.cols[[i]]]),1,prod)
}

plotdata[, effect := res]

# plotting groups
plotdata[type=='none', typetext := 'No Influence']
plotdata[type=='trade', typetext := 'Influenced']
plotdata[, tltext := ifelse(tl.user==1,'Leader','Trader')]
plotdata[, grp := paste0(typetext,'/',tltext)]

# set factor order
plotdata[, grp := factor(grp,unique(grp))]

plotdata <- plotdata[type %in% c('trade','none')]
#plotdata <- plotdata[!(type == 'none' & nfriends == 0)]

# test.pred <- predict(object=basem11b.l, newdata=plotdata,
#                         type='expected', reference='sample')


graph <- ggplot(plotdata,aes(x=rank,y=effect,linetype=grp,color=grp)) + 
    scale_linetype_manual(values=rep(c('solid','22'),each=2)) +
    scale_color_manual(values=rep(brewer.pal(3,'PuBu')[2:3],2)) +
    labs(y='Log Odds Ratio',x='Currency Pair Frequency Rank',
         color=NULL,linetype=NULL) +
    theme_minimal() +
    theme(legend.position=c(1,0),legend.justification=c(1,0)) +
    geom_line(lwd=1)

pngres <- 300
wid <- 6
hgt <- 4
png('sudocox-tlxp.png',res=pngres,width=wid*pngres,height=hgt*pngres)
graph
dev.off()

postscript('sudocox-tlxp.eps',width=wid,height=hgt)
graph
dev.off()



