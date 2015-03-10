library(survival)
library(data.table)
library(ggplot2)
library(RColorBrewer)

setwd('~/Data/forex')
load('Rdata/adopt-analysis-sudocox-cp-friends.Rdata')

rm(basem11b.base,basem11b,basem11b.5,basem11b.10,basem11b.20)
gc()

setwd('~/2YP/writing/plots/')
 
# # running quantile on the original data:
# > quantile(all.adopts$nfriends,seq(0,1,0.01))
#   0%   1%   2%   3%   4%   5%   6%   7%   8%   9%  10%
#    0    0    0    0    0    0    0    0    0    0    0
#  11%  12%  13%  14%  15%  16%  17%  18%  19%  20%  21%
#    0    0    0    0    0    0    0    0    0    0    0
#  22%  23%  24%  25%  26%  27%  28%  29%  30%  31%  32%
#    0    0    0    0    0    0    0    0    0    0    0
#  33%  34%  35%  36%  37%  38%  39%  40%  41%  42%  43%
#    0    0    0    0    0    0    0    0    0    1    1
#  44%  45%  46%  47%  48%  49%  50%  51%  52%  53%  54%
#    1    1    1    1    1    1    2    2    2    2    2
#  55%  56%  57%  58%  59%  60%  61%  62%  63%  64%  65%
#    2    3    3    3    3    3    4    4    4    5    5
#  66%  67%  68%  69%  70%  71%  72%  73%  74%  75%  76%
#    5    6    6    6    7    7    8    8    9    9   10
#  77%  78%  79%  80%  81%  82%  83%  84%  85%  86%  87%
#   10   11   11   12   13   14   15   16   17   18   19
#  88%  89%  90%  91%  92%  93%  94%  95%  96%  97%  98%
#   20   22   24   26   28   32   36   42   49   64   88
#  99% 100%
#  144 1523

# so, 0, median and 95% seem like good places to plot at

setEPS()

######################################################################

plotdata <- CJ(type=c('none','trade','profit'),
               #ntgt0.a14=c(FALSE,TRUE), ndpos.a14=c(FALSE,TRUE),
               rank=1:133, nfriends=c(0,2,42))

# proper parameters
plotdata[type == 'none', `:=`(ntgt0.a14=0, ndpos.a14=0)]
plotdata[type == 'trade', `:=`(ntgt0.a14=1, ndpos.a14=0)]
plotdata[type == 'profit', `:=`(ntgt0.a14=1, ndpos.a14=1)]
plotdata[, rank.l := log(rank)]
plotdata[, nfriends.l := log(1 + nfriends)]


# make predictions
coefs <- basem11b.l$coefficients
ses <- sqrt(diag(basem11b.l$var))

# get rid of NAs
coefs[is.na(coefs)] <- 0

vars <- as.matrix(plotdata[,list(ntgt0.a14,ndpos.a14,rank.l,nfriends.l)])
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
plotdata[nfriends == 0, friendtext := 'No Friends']
plotdata[nfriends == 2, friendtext := 'Median Friends']
plotdata[nfriends == 42, friendtext := '95%ile Friends']
plotdata[, grp := paste0(typetext,'/',friendtext)]

# set factor order
plotdata[, grp := factor(grp,unique(grp))]

plotdata <- plotdata[type %in% c('trade','none')]
plotdata <- plotdata[nfriends>0]
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
png('sudocox-cp-robustness.png',res=pngres,width=wid*pngres,height=hgt*pngres)
graph
dev.off()

postscript('sudocox-cp-robustness.eps',width=wid,height=hgt)
graph
dev.off()


