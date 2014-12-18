library(data.table)
library(ggplot2)
library(mgcv)
library(ffbase)
library(reshape2)

rm(list=ls())

setwd('~/Data/Currensee')

aus <- readRDS('active-user-quantiles.Rds')
dt <- readRDS('day.stats-0-1samp.Rds')
fpt <- readRDS('forexposition.Rds')
ffdfns <- load.ffdf('./ffdb/sbc')
ffd <- ffdfns$ffd

summary(dt)
summary(aus)
summary(fpt)
summary(ffd)

# put days in fpt stuff
fpt[,openday := floor((opendate / 86400000.0) + 0.125)]
fpt[,cp := paste0(currency1,currency2)]

# get currency use frequency
cps <- fpt[, list(cpN=.N), by=cp]
cps <- cps[order(-cps$cpN)]
cps[,rank:=.I]
cps[,major5 := as.numeric(rank <= 5)]
cps[,major10 := as.numeric(rank <= 10)]
cps[,major15 := as.numeric(rank <= 15)]
cps[,major20 := as.numeric(rank <= 20)]

# merge in currency frequency
setkey(cps,cp)
setkey(fpt,cp)
fpt <- merge(fpt,cps,all.x=TRUE)

# restrict to meaningful users
uids <- aus[Npd_q %in% 4:7 & med_ob_q %in% 4:7 & dpnlpd_q %in% 2:9,user_id]

c.dt <- dt[user_id %in% uids]
c.fpt <- fpt[user_id %in% uids]

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



# want to look at major open status, not just boolean open status...
open.cps <- c.fpt[,list(highrank=min(rank),lowrank=max(rank)),by=list(user_id,day=openday)]
setkey(c.dt,user_id,day)
c.dt <- merge(c.dt, open.cps, all.x=TRUE)

# make cp-rank open-trade booleans
for (i in c(5,10,20)) {
    c.dt[bopen==1, paste0(c('bh','bl'), i) := list(
        as.integer(highrank <= i), as.integer(highrank > i))]
    c.dt[bopen==0, paste0(c('bh','bl'), i) := list(0, 0)]
}



# pull out relevant users from daily currency data stuff
ptm <- system.time(
    idx.uids <- ffwhich(ffd,user_id %in% uids & ))
ptm
# looking at users only, have ~ 23,152,633 (in 11 sec)

# let's see if this fits in a dt
ptm <- system.time(
    sbcdt <- as.data.table(as.data.frame(ffd[idx.uids,])))
ptm
# abt 96 sec, takes ~ 1.5 gb, 4 gb total in ram for some reason...
gc()
# now 1.98 gb, cool


# drop out some of the gaps
sbcdt <- sbcdt[gap %in% c(3,7)]

# merge in cp ranks
setkey(sbcdt,cp)
sbcdt <- merge(sbcdt,cps)
gc()

# look at partitions by rank
c.sbcdt <- sbcdt[,list(rows_smushed=.N,
                       ntotal=sum(ntotal),
                       npos=sum(npos),
                       nneg=sum(nneg),
                       dpnl_sum=sum(dpnl_sum),
                       dpnl_pos=sum(dpnl_pos),
                       dpnl_neg=sum(dpnl_neg)
                       ),by=list(user_id,day,gap,type,major=major10)]
c.sbcdt[,dpnl_mean:=dpnl_sum/ntotal]
rm(sbcdt); gc()

c.sbcdt[,rows_smushed := NULL]
m.sbcdt <- melt(c.sbcdt, id.vars=c('user_id','day','gap','type','major'))

# rename levels
m.sbcdt[, major := ifelse(major==1,'maj','min')]
sbc.wide <- dcast.data.table(m.sbcdt, user_id + day ~ variable + type + major + gap)

rm(m.sbcdt,c.sbcdt); gc()



# check that all rows exist
# slow
# sbc.wide[,list(exists = dim(c.dt[user_id==c.uid & day==c.day])[1] > 0),by=list(c.uid=user_id,c.day=day)]
c.dt.keys <- c.dt[,list(user_id,day)]

setkey(c.dt.keys,user_id,day)
setkey(c.dt,user_id,day)
setkey(sbc.wide,user_id,day)

print(dim(merge(c.dt.keys,sbc.wide))[1])
print(dim(sbc.wide)[1])

c.sbc.wide <- merge(c.dt.keys,sbc.wide, all.x=TRUE)

# set NA's to 0, h/t Matt Dowle, https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
for (j in names(c.sbc.wide))
    set(c.sbc.wide,which(is.na(c.sbc.wide[[j]])),j,0.0)

summary(c.sbc.wide)
setkey(c.sbc.wide,user_id,day)

c.dt <- merge(c.dt,c.sbc.wide)

rm(fpt, sbc.wide, c.sbc.wide, c.dt.keys, dt); gc()

set.seed(1)
c.dt[,slc := runif(.N)]



# visual analysis
setwd('~/Dropbox/Currensee Project/2YP/plots/min-maj/')

crit.SLC <- 1.0

# ============================================================
# mean dpnl
png('ego_mdpnl_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_ego_maj_7 / med_ob,bh10)) + 
    labs(x="Ego: Mean $ PNL / Median Balance (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_mdpnl_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_ego_maj_7 / med_ob,bl10)) + 
    labs(x="Ego: Mean $ PNL / Median Balance (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_mdpnl_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_ego_min_7 / med_ob,bh10)) + 
    labs(x="Ego: Mean $ PNL / Median Balance (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_mdpnl_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_ego_min_7 / med_ob,bl10)) + 
    labs(x="Ego: Mean $ PNL / Median Balance (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()


# ============================================================
# total dpnl
png('ego_tdpnl_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_ego_maj_7 / med_ob,bh10)) + 
    labs(x="Ego: Total $ PNL / Median Balance (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_tdpnl_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_ego_maj_7 / med_ob,bl10)) + 
    labs(x="Ego: Total $ PNL / Median Balance (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_tdpnl_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_ego_min_7 / med_ob,bh10)) + 
    labs(x="Ego: Total $ PNL / Median Balance (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_tdpnl_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_ego_min_7 / med_ob,bl10)) + 
    labs(x="Ego: Total $ PNL / Median Balance (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()



# ============================================================
# excess wins
png('ego_exwin_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_ego_maj_7 - nneg_ego_maj_7,bh10)) + 
    labs(x="Ego: Excess Wins (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_ego_maj_7 - nneg_ego_maj_7,bl10)) + 
    labs(x="Ego: Excess Wins (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_ego_min_7 - nneg_ego_min_7,bh10)) + 
    labs(x="Ego: Excess Wins (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_ego_min_7 - nneg_ego_min_7,bl10)) + 
    labs(x="Ego: Excess Wins (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()


# ============================================================
# excess win fraction
png('ego_exwin_frac_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_ego_maj_7 - nneg_ego_maj_7) / ntotal_ego_maj_7,bh10)) + 
    labs(x="Ego: Excess Win Fraction (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_frac_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_ego_maj_7 - nneg_ego_maj_7) / ntotal_ego_maj_7,bl10)) + 
    labs(x="Ego: Excess Win Fraction (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_frac_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_ego_min_7 - nneg_ego_min_7) / ntotal_ego_min_7,bh10)) + 
    labs(x="Ego: Excess Win Fraction (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('ego_exwin_frac_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_ego_min_7 - nneg_ego_min_7) / ntotal_ego_min_7,bl10)) + 
    labs(x="Ego: Excess Win Fraction (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()





# ============================================================
# ============================================================
# ALTERS
# ============================================================
# ============================================================

# ============================================================
# mean dpnl
png('alter_mdpnl_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_alter_maj_7 / med_ob,bh10)) + 
    labs(x="Alter: Mean $ PNL / Median Balance (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_mdpnl_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_alter_maj_7 / med_ob,bl10)) + 
    labs(x="Alter: Mean $ PNL / Median Balance (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_mdpnl_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_alter_min_7 / med_ob,bh10)) + 
    labs(x="Alter: Mean $ PNL / Median Balance (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_mdpnl_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_mean_alter_min_7 / med_ob,bl10)) + 
    labs(x="Alter: Mean $ PNL / Median Balance (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()


# ============================================================
# total dpnl
png('alter_tdpnl_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_alter_maj_7 / med_ob,bh10)) + 
    labs(x="Alter: Total $ PNL / Median Balance (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_tdpnl_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_alter_maj_7 / med_ob,bl10)) + 
    labs(x="Alter: Total $ PNL / Median Balance (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_tdpnl_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_alter_min_7 / med_ob,bh10)) + 
    labs(x="Alter: Total $ PNL / Median Balance (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_tdpnl_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=dpnl_sum_alter_min_7 / med_ob,bl10)) + 
    labs(x="Alter: Total $ PNL / Median Balance (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()



# ============================================================
# excess wins
png('alter_exwin_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_alter_maj_7 - nneg_alter_maj_7,bh10)) + 
    labs(x="Alter: Excess Wins (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_alter_maj_7 - nneg_alter_maj_7,bl10)) + 
    labs(x="Alter: Excess Wins (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_alter_min_7 - nneg_alter_min_7,bh10)) + 
    labs(x="Alter: Excess Wins (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=npos_alter_min_7 - nneg_alter_min_7,bl10)) + 
    labs(x="Alter: Excess Wins (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()


# ============================================================
# excess win fraction
png('alter_exwin_frac_maj-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_alter_maj_7 - nneg_alter_maj_7) / ntotal_alter_maj_7,bh10)) + 
    labs(x="Alter: Excess Win Fraction (Major)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_frac_min-on-maj.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_alter_maj_7 - nneg_alter_maj_7) / ntotal_alter_maj_7,bl10)) + 
    labs(x="Alter: Excess Win Fraction (Major)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_frac_maj-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_alter_min_7 - nneg_alter_min_7) / ntotal_alter_min_7,bh10)) + 
    labs(x="Alter: Excess Win Fraction (Minor)", y="p(Open Major)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

png('alter_exwin_frac_min-on-min.png', width=800, height=600)
ggplot(c.dt[slc<crit.SLC],aes(x=(npos_alter_min_7 - nneg_alter_min_7) / ntotal_alter_min_7,bl10)) + 
    labs(x="Alter: Excess Win Fraction (Minor)", y="p(Open Minor)") +
    geom_jitter(alpha=0.2, position=position_jitter(height=0.1)) + 
    stat_smooth(method='gam', family='binomial')
gc(verbose=FALSE); dev.off()

setwd('~/Data/Currensee')
