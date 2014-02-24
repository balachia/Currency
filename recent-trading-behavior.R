library(data.table)
library(ggplot2)
library(mgcv)
library(ffbase)

rm(list=ls())

setwd('~/Data/Currensee')

aus <- readRDS('active-user-quantiles.Rds')
dt <- readRDS('day.stats-0-1samp.Rds')
ffdfns <- load.ffdf('./ffdb/sbc')
ffd <- ffdfns$ffd

summary(dt)
summary(aus)
summary(ffd)

# looking at users of ~ once a week - once a day
aus[,list(min=min(Npd),
          q1=quantile(Npd,0.25),
          med=median(Npd),
          mean=mean(Npd),
          q3=quantile(Npd,0.75),
          max=max(Npd)
        ),by=Npd_q][order(Npd_q)]
aus[,list(min=min(med_ob),
          q1=quantile(med_ob,0.25),
          med=median(med_ob),
          mean=mean(med_ob),
          q3=quantile(med_ob,0.75),
          max=max(med_ob)
        ),by=med_ob_q][order(med_ob_q)]
aus[,list(min=min(dpnlpd),
          q1=quantile(dpnlpd,0.25,na.rm=TRUE),
          med=median(dpnlpd),
          mean=mean(dpnlpd),
          q3=quantile(dpnlpd,0.75,na.rm=TRUE),
          max=max(dpnlpd)
        ),by=dpnlpd_q][order(dpnlpd_q)]

uids <- aus[Npd_q %in% 4:7 & med_ob_q %in% 4:7 & dpnlpd_q %in% 2:9,user_id]

c.dt <- dt[user_id %in% uids]

# preprocess c.dt stuff
c.dt[,bopen:=as.integer(opened_today>0)]

c.dt <- c.dt[order(user_id,day)]
c.dt[,daygroup:=cumsum(bopen) - bopen, by=user_id]
c.dt[,daylapse := .I - .I[1] + 1.0, by=list(user_id,daygroup)]
c.dt[,lapsegrp := floor(log2(1+log2(daylapse)))]
c.dt[,lapsegrp2 := ifelse(daylapse < 6, daylapse, 6)]
c.dt[,lapsegrp3 := floor(log(daylapse,3))]

# merge in user statistics
setkey(c.dt,user_id)
setkey(aus,user_id)

c.dt <- merge(c.dt,aus)



# pull out relevant users from daily currency data stuff
ptm <- system.time(
    idx.uids <- ffwhich(ffd,user_id %in% uids))
ptm
# looking at users only, have ~ 23,152,633 (in 11 sec)

# let's see if this fits in a dt
ptm <- system.time(
    sbcdt <- as.data.table(as.data.frame(ffd[idx.uids,])))
ptm
# abt 96 sec, takes ~ 1.5 gb, 4 gb total in ram for some reason...
gc()
# now 1.98 gb, cool

# how then, shall we collapse this monster
gaps <- unique(sbcdt$gap)
types <- unique(sbcdt$type)

# exclude or label relevant currency pairs, at this point lumping all
c.sbcdt <- sbcdt[,list(rows_smushed=.N,
                       ntotal=sum(ntotal),
                       npos=sum(npos),
                       nneg=sum(nneg),
                       dpnl_sum=sum(dpnl_sum),
                       dpnl_pos=sum(dpnl_pos),
                       dpnl_neg=sum(dpnl_neg)
                       ),by=list(user_id,day,gap,type)]
c.sbcdt[,dpnl_mean:=dpnl_sum/ntotal]

# can probably rm(sbcdt) at this point
rm(sbcdt); gc()

# reshape to wide via gap, type
sbc.wide <- c.sbcdt[,.N,by=list(user_id,day)]
sbc.wide[,N:=NULL]
setkey(sbc.wide,user_id,day)

for (c.type in types) {
    for (c.gap in gaps) {
        cat(c.type, ' :: ', c.gap, '\n')
        tmp.sbcdt <- c.sbcdt[type==c.type & gap==c.gap,]
        tmp.sbcdt[,c('gap','type','rows_smushed') := NULL]
        
#         print(tmp.sbcdt)
        
        setnames(tmp.sbcdt,names(tmp.sbcdt)[3:9], paste0(names(tmp.sbcdt)[3:9], '_', c.type, c.gap))
        setkey(tmp.sbcdt,user_id,day)
        
        sbc.wide <- merge(sbc.wide,tmp.sbcdt,all.x=TRUE)
    }
}

summary(sbc.wide)

# check that all rows exist
# slow
# sbc.wide[,list(exists = dim(c.dt[user_id==c.uid & day==c.day])[1] > 0),by=list(c.uid=user_id,c.day=day)]
c.dt.keys <- c.dt[,list(user_id,day)]

setkey(c.dt.keys,user_id,day)
setkey(c.dt,user_id,day)
setkey(sbc.wide,user_id,day)

print(dim(merge(c.dt.keys,sbc.wide))[1])
print(dim(sbc.wide)[1])

c.sbc.wide <- merge(c.dt.keys,sbc.wide,all.x=TRUE)

# set NA's to 0, h/t Matt Dowle, https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
for (j in names(c.sbc.wide))
    set(c.sbc.wide,which(is.na(c.sbc.wide[[j]])),j,0.0)

summary(c.sbc.wide)
setkey(c.sbc.wide,user_id,day)

c.dt <- merge(c.dt,c.sbc.wide)

# and done with that BS...





# analysis parts

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob),
            family='binomial', data=c.dt))
qplot(c.dt$hadopen,predict(res))
qplot(c.dt$hadopen,resid(res, type='deviance'))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl),
            family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl) + I(scale(totaldpnl/med_ob)),
            family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
            + I(scale((posdpnl) / (posdpnl - negdpnl))),
            family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
            + I(scale((posdpnl) / (posdpnl - negdpnl))),
            family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
            + I(scale((posdpnl) / (posdpnl - negdpnl)))
            + scale(dpnl_mean_ego7),
            family='binomial', data=c.dt))
qplot(c.dt[!res$na.action]$dpnl_mean_ego7,predict(res))
qplot(c.dt[!res$na.action]$dpnl_mean_ego7,resid(res, type='deviance'))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
            + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
            + I(scale((posdpnl) / (posdpnl - negdpnl)))
            + s(scale(dpnl_mean_ego7)),
            family='binomial', data=c.dt))
qplot(c.dt[!res$na.action]$dpnl_mean_ego7,predict(res))
qplot(c.dt[!res$na.action]$dpnl_mean_ego7,resid(res, type='deviance'))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_mean_ego7 / med_ob)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + scale(dpnl_sum_ego7),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)),
                   family='binomial', data=c.dt))

# both mean and sum
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob))
                   + I(scale(dpnl_mean_ego7 / med_ob)),
                   family='binomial', data=c.dt))

# both splined
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + s(I(scale(dpnl_sum_ego7 / med_ob)))
                   + s(I(scale(dpnl_mean_ego7 / med_ob))),
                   family='binomial', data=c.dt))
qplot(c.dt[!res$na.action,dpnl_sum_ego7 / med_ob],predict(res))
qplot(c.dt[!res$na.action,dpnl_mean_ego7 / med_ob],predict(res))
qplot(c.dt[!res$na.action]$dpnl_mean_ego7,resid(res, type='deviance'))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * I(scale(dpnl_mean_ego7 / med_ob)),
                   family='binomial', data=c.dt))

# interact w # trades
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob))
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + scale(ntotal_ego7),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob))
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + scale(ntotal_ego7),
                   family='binomial', data=c.dt))


summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_mean_ego7 / med_ob)) * scale(ntotal_ego7),
                   family='binomial', data=c.dt))

# raw win/gain numbers
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(npos_ego7 / ntotal_ego7)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(npos_ego7 - nneg_ego7)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale((npos_ego7 - nneg_ego7) / ntotal_ego7)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + scale(ntotal_ego7),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + scale(npos_ego7) * scale(nneg_ego7)
                   + scale(npos_ego7) * I(scale(dpnl_mean_ego7 / med_ob))
                   + scale(nneg_ego7) * I(scale(dpnl_mean_ego7 / med_ob)),
                   family='binomial', data=c.dt))


# bigass combos
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + scale(totaldpnl) + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob)),
                   family='binomial', data=c.dt))
qplot(c.dt[!res$na.action,dpnl_sum_ego7 / med_ob],plogis(predict(res))) + stat_smooth(family='binomial')

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + I(scale((npos_ego7 - nneg_ego7) / ntotal_ego7)),
                   family='binomial', data=c.dt))


# just gam the thing
summary(res <- gam(bopen~s(hadopen) + s(med_ob) + s(totaldpnl)
                   + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + I(scale((npos_ego7 - nneg_ego7) / ntotal_ego7)),
                   family='binomial', data=c.dt))
qplot(c.dt[!res$na.action,hadopen],plogis(predict(res)))
qplot(c.dt[!res$na.action,med_ob],plogis(predict(res)))
qplot(c.dt[!res$na.action,totaldpnl],plogis(predict(res)))

# alters
summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + I(scale((npos_ego7 - nneg_ego7) / ntotal_ego7))
                   + I(scale(dpnl_sum_alter7 / med_ob))
                   + I(scale(dpnl_mean_alter7 / med_ob)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + I(scale((npos_ego7 - nneg_ego7) / ntotal_ego7))
                   + I(scale(dpnl_sum_alter7 / med_ob)) * scale(ntotal_alter7)
                   + I(scale(dpnl_mean_alter7 / med_ob)),
                   family='binomial', data=c.dt))

summary(res <- gam(bopen~poly(hadopen,2) + scale(med_ob)
                   + I(scale(totaldpnl/med_ob))
                   + I(scale((posdpnl) / (posdpnl - negdpnl)))
                   + I(scale(dpnl_sum_ego7 / med_ob)) * scale(ntotal_ego7)
                   + I(scale(dpnl_mean_ego7 / med_ob))
                   + I(scale(npos_ego7 / ntotal_ego7))
                   + I(scale(dpnl_sum_alter7 / med_ob)) * scale(ntotal_alter7)
                   + I(scale(dpnl_mean_alter7 / med_ob))
                   + I(scale(npos_alter7 / ntotal_alter7)),
                   family='binomial', data=c.dt))




setwd('~/Dropbox/Currensee Project/2YP/plots/recent-hist/')
png('ego_mdpnl.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_mean_ego7,bopen)) + 
    labs(x="Ego: Mean $ PNL") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('ego_tdpnl.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_sum_ego7,bopen)) + 
    labs(x="Ego: Total $ PNL") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_mdpnl.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_mean_alter7,bopen)) + 
    labs(x="Alter: Mean $ PNL") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_tdpnl.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_sum_alter7,bopen)) + 
    labs(x="Alter: Total $ PNL") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()



png('ego_mdpnl_ob.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_mean_ego7/med_ob,bopen)) + 
    labs(x="Ego: Mean $ PNL / Median Balance") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('ego_tdpnl_ob.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_sum_ego7/med_ob,bopen)) + 
    labs(x="Ego: Total $ PNL / Median Balance") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('ego_dpnl_frac.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_pos_ego7/(dpnl_pos_ego7 - dpnl_neg_ego7),bopen)) + 
    labs(x="Ego: $ Fraction Gained") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_dpnl_frac.png', width=800, height=600)
ggplot(c.dt,aes(x=dpnl_pos_alter7/(dpnl_pos_alter7 - dpnl_neg_alter7),bopen)) + 
    labs(x="Alter: $ Fraction Gained") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('ego_npfrac.png', width=800, height=600)
ggplot(c.dt,aes(x=npos_ego7/ntotal_ego7,bopen)) + 
    labs(x="Ego: # Fraction Won") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_npfrac.png', width=800, height=600)
ggplot(c.dt,aes(x=npos_alter7/ntotal_alter7,bopen)) + 
    labs(x="Alter: # Fraction Won") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()




png('ego_exwin.png', width=800, height=600)
ggplot(c.dt,aes(x=npos_ego7 - nneg_ego7,bopen)) + 
    labs(x="Ego: # Won - # Lost") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_exwin.png', width=800, height=600)
ggplot(c.dt,aes(x=npos_alter7 - nneg_alter7,bopen)) + 
    labs(x="Alters: # Won - # Lost") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('ego_exwin_frac.png', width=800, height=600)
ggplot(c.dt,aes(x=(npos_ego7 - nneg_ego7)/ntotal_ego7,bopen)) + 
    labs(x="Ego: # Excess Wins / Total") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()


png('alter_exwin_frac.png', width=800, height=600)
ggplot(c.dt,aes(x=(npos_alter7 - nneg_alter7)/ntotal_alter7,bopen)) + 
    labs(x="Alter: # Excess Wins / Total") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
dev.off()




setwd('~/Data/Currensee')
