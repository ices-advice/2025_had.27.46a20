## Extract results of interest, write TAF output tables

## Before:
## After:

graphics.off()

load("model/SAM/NShaddock_WGNSSK2024_Run2/model.RData") # last year's
prev_ass <- fit

# load this year's assessment and forecast
load("model/SAM/NShaddock_WGNSSK2025_Run1/model.RData")
load("model/SAM/NShaddock_WGNSSK2025_Run1/leaveout.RData")
load("model/SAM/NShaddock_WGNSSK2025_Run1/residuals.RData")
load("model/SAM/NShaddock_WGNSSK2025_Run1/retro.RData")


# summary tables for input data ---------------------------------------------------------------------

output.dir <-"output/input data/"

# discards numbers table

df <- as.data.frame(stock.data@discards.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, paste0(output.dir,"tab_discards.n.csv"))

# landings numbers table
df <- as.data.frame(stock.data@landings.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, paste0(output.dir,"tab_landings.n.csv"))

# catch numbers table
df <- as.data.frame(stock.data@catch.n)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, paste0(output.dir,"tab_catch.n.csv"))

# bms numbers table
df <- as.data.frame(bmsn)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, paste0(output.dir,"tab_bms.n.csv"))

# ibc numbers table
df <- as.data.frame(ibcn)
df <- reshape(df, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
write.taf(df, paste0(output.dir,"tab_ibc.n.csv"))

# Indices
idx1 <- x.idx[[1]]@index
df1 <- as.data.frame(idx1)
df1 <- reshape(df1, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
df1$Quarter <- "Q1"

idx2 <- x.idx[[2]]@index
df2 <- as.data.frame(idx2)
df2 <- reshape(df2, idvar = "year", timevar = "age", v.names = "data", direction = "wide")
df2$Quarter <- "Q3+Q4"

df <- merge(x = df1, y = df2, all = TRUE)
names(df) <- gsub(pattern = "data.", replacement = "", x = names(df), fixed = TRUE)
names(df) <- gsub(pattern = "year", replacement = "Year", x = names(df), fixed = TRUE)

df <- df[,c("Year","Quarter", ac(0:8))]
write.taf(df, paste0(output.dir,"tab_indices.csv"))



# SAM tables ----------------------------------------------

output.dir <- "output/SAM/"

# summary table =====#
tsb <- tsbtable(fit)
colnames(tsb)<-c("TSB","Low", "High")
tab_summary <- cbind(summary(fit), tsb)
tab_summary <- xtab2taf(tab_summary)
write.taf(tab_summary, paste0(output.dir,"tab_summary.csv"))

# F at age table =====#
tab_fay <- faytable(fit)
tab_fay <- xtab2taf(tab_fay)
write.taf(tab_fay, paste0(output.dir,"tab_fay.csv"))

# catch table =====#
tab_catch <- catchtable(fit)
colnames(tab_catch) <- c("Catch","Low", "High")
tab_catch <- xtab2taf(tab_catch)
write.taf(tab_catch, paste0(output.dir,"tab_catch.csv"))

# numbers table =====#
tab_numbers <- ntable(fit)
tab_numbers <- xtab2taf(tab_numbers)
write.taf(tab_numbers, paste0(output.dir,"tab_numbers.csv"))

# parameter table =====#
tab_par <- partable(fit)
tab_par <- cbind(data.frame("Parameter name" = rownames(tab_par), check.names = FALSE), tab_par)
write.taf(tab_par, paste0(output.dir,"tab_pars.csv"))

# parameter sd table =====#
sdState <- function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR <- fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB <- fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF <- fit$sdrep$sd[idx][fit$data$years%in%y]
  ret <- cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret) <- y
  colnames(ret) <- c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}
tab_sd <- sdState(fit)
tab_sd <- xtab2taf(tab_sd)
write.taf(tab_sd, paste0(output.dir,"tab_sd.csv"))

# model estimated catch numbers
SAM_catch.n <- getFleet(fit,fleet=1,pred = T)
SAM_landings.n <- SAM_catch.n*fit$data$landFrac[,,1]
SAM_discards.n <- SAM_catch.n*(1-fit$data$landFrac[,,1])
#check
sum(round(SAM_catch.n-SAM_landings.n-SAM_discards.n,5))

# SAM yields
SAM_landings <- rowSums(SAM_landings.n*fit$data$landMeanWeight[,,1])
SAM_discards <- rowSums(SAM_discards.n*fit$data$disMeanWeight[,,1])

# save
write.taf(SAM_catch.n, paste0(output.dir,"tab_catch_n.csv"))
write.taf(SAM_landings.n, paste0(output.dir,"tab_landings_n.csv"))
write.taf(SAM_discards.n, paste0(output.dir,"tab_discards_n.csv"))
write.taf(t(SAM_landings), paste0(output.dir,"tab_landings.csv"),row.names=T)
write.taf(t(SAM_discards), paste0(output.dir,"tab_discards.csv"),row.names=T)


# Standard SAM plots --------------------------------------------------------------------------------------------
output.dir <- "output/SAM/"

# stock summary
taf.png(paste0(output.dir,"summary.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
plot(fit, partial = FALSE, las = 0, xlab = "")
dev.off()

# ssb
taf.png(paste0(output.dir,"ssb.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
ssbplot(fit, ci = TRUE, las = 0, xlab = "",main="SSB")
lines(1972:ay,rep(Btrig,length(1972:ay)),lty=2,col="blue",lwd=2)
lines(1972:ay,rep(Blim,length(1972:ay)),lty=2,col="orange",lwd=2)

legend("bottomleft",inset=0.02,c("MSY Btrigger/Bpa","Blim"),lty=2,lwd=2,col=c("blue","orange"),bg = "white")
dev.off()


# Fbar
taf.png(paste0(output.dir,"fbar.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
fbarplot(fit, partial = FALSE, las = 0, xlab = "",main="Fbar (2-4)")
lines(1972:ay,rep(Fmsy,length(1972:ay)),lty=2,col="blue",lwd=2)

legend("bottomleft",inset=0.02,c("FMSY"),lty=2,lwd=2,col=c("blue"),bg = "white")


dev.off()

# F at age
fay <- reshape2::melt(faytable(fit))
names(fay) <- c("Year","age","fay")
fay <- fay[fay$Year<ay,]

png(paste0("output/SAM/F at age by year.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=fay, aes(Year, fay,group=age,colour=as.factor(age))) +
  geom_line() + theme_bw()+ labs(y="F-at-age",x="",colour="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

png(paste0("output/SAM/F at age by decade.png"),width = 11, height = 7, units = "in", res = 600)

fay$decade <- NA
fay$decade<- paste0(substr(ac(fay$Year),start=1,stop = 3),"0")
fay$dec_yr <- substr(ac(fay$Year),start=4,stop = 4)

p1 <- ggplot(data=fay, aes(age, fay,group=dec_yr,colour=as.factor(dec_yr))) +
  facet_wrap(~decade)+
  geom_line() + theme_bw()+ labs(y="F-at-age",x="",colour="year of decade") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# Selectivity
Fsel <- faytable(fit)/rowSums(faytable(fit))
Fsel <- reshape2::melt(Fsel)
names(Fsel) <- c("Year","age","sel")
Fsel <- Fsel[Fsel$Year<ay,]

png(paste0("output/SAM/Selectivity at age by year.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=Fsel, aes(Year, sel,group=age,colour=as.factor(age))) +
  geom_line() + theme_bw()+ labs(y="Selectivity-at-age",x="",colour="") +
  theme(axis.title=element_text(size=10),axis.text=element_text(size=9),
        legend.text=element_text(size=10)) 
print(p1)
dev.off()

Fsel$decade <- NA
Fsel$decade<- paste0(substr(ac(Fsel$Year),start=1,stop = 3),"0")
Fsel$dec_yr <- substr(ac(Fsel$Year),start=4,stop = 4)

png(paste0("output/SAM/Selectivity at age by decade.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=Fsel, aes(age, sel,group=dec_yr,colour=as.factor(dec_yr))) +
  facet_wrap(~decade)+
  geom_line() + theme_bw()+ labs(y="Selectivity-at-age",x="",colour="year of decade") +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=9),
        legend.text=element_text(size=10)) 
print(p1)
dev.off()

# for iceland haddock high variability in selection at age? 
#is this because of growth DD and therefore affects age of recruitment to fishery?
# plot selection minus long term mean to compare to mean wt?

# Recruitment
taf.png(paste0("output/SAM/rec.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
recplot(fit, las = 0, xlab = "",main="Rec (age 0)",drop=1)
dev.off()

taf.png(paste0("output/SAM/sr.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
srplot(fit)
dev.off()

taf.png(paste0("output/SAM/sr_alt.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
srplot(fit,CIlevel=0)
dev.off()

taf.png(paste0("output/SAM/sr_short_ts.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
rec <- as.data.frame(rectable(fit))[as.character(2000:(ay-1)),"Estimate"]
ssb <- as.data.frame(ssbtable(fit))[as.character(2000:(ay-1)),"Estimate"]
yr_lab <- as.character(2000:(ay-1))
plot(ssb,rec,type="l",xlab="SSB (tonnes)",ylab="Recruitment (age 0) (thousands)")
text(ssb,rec,labels=yr_lab,col="red",cex=0.7)
dev.off()

taf.png(paste0("output/SAM/sr_long_ts.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
rec <- as.data.frame(rectable(fit))[as.character(1972:(ay-1)),"Estimate"]
ssb <- as.data.frame(ssbtable(fit))[as.character(1972:(ay-1)),"Estimate"]
yr_lab <- as.character(1972:(ay-1))
plot(ssb,rec,type="l",xlab="SSB (tonnes)",ylab="Recruitment (age 0) (thousands)")
text(ssb,rec,labels=yr_lab,col="red",cex=0.7)
dev.off()

taf.png(paste0("output/SAM/sr_with_Blim.png"), width = WIDTH, height = HEIGHT, units = UNITS, res = RESO)
rec <- as.data.frame(rectable(fit))[,"Estimate"]
ssb <- as.data.frame(ssbtable(fit))[,"Estimate"]
Blim <- ssb[ts_yrs == 1999]
Rblim <- rec[ts_yrs == 1999]
plot(ssb,rec,type="p",xlab="SSB (tonnes)",ylab="Recruitment (age 0) (thousands)")
points(ssb[ts_yrs>1999],rec[ts_yrs>1999],pch=16,col="black")
points(Blim,Rblim,pch=16,col="red")
lines(rep(Blim,length(rec)),rec,col="red")
legend("topright",inset=0.02,legend=c("Rec all yrs","Rec 2000+","Blim"),pch=c(1,16,16),lty=c(NA,NA,1),col=c("black","black","red"))
dev.off()

# CV on rec
temp <- rectable(fit)
SD   <- (log(temp[,"High"])-log(temp[,"Low"]))/4
SD_idx <- which(ts_yrs<ay)
temp <- rectable(prev_ass)
SD_prev   <- (log(temp[,"High"])-log(temp[,"Low"]))/4
SD_prev_idx <- which(ts_yrs<(ay-1))

taf.png(paste0("output/SAM/Recruiment CV with int year.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
plot(ts_yrs,SD,type="o",pch=16,main="CV on Recruitment",ylim=c(0,max(SD,SD_prev)))
lines(ts_yrs[SD_idx],SD_prev[SD_idx],type="o",pch=16,col="grey50")
legend("topleft",inset=0.02,legend=c(paste0("WGNSSK ",ay),paste0("WGNSSK ",ay-1)),col=c("black","grey50"),pch=16,lty=1)
dev.off()

temp <- rectable(fit)
SD   <- (log(temp[,"High"])-log(temp[,"Low"]))/4
SD_idx <- which(ts_yrs<ay)
temp <- rectable(prev_ass)
SD_prev   <- (log(temp[,"High"])-log(temp[,"Low"]))/4
SD_prev_idx <- which(ts_yrs<(ay-1))

taf.png(paste0("output/SAM/Recruiment CV.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
plot(ts_yrs[SD_idx],SD[SD_idx],type="o",pch=16,main="CV on Recruitment",#ylim=c(0,max(SD,SD_prev)),
     xlab="",ylab="CV",ylim=c(0,0.5))
lines(ts_yrs[SD_prev_idx],SD_prev[SD_prev_idx],type="o",pch=16,col="grey50")
legend("topleft",inset=0.02,legend=c(paste0("WGNSSK ",ay),paste0("WGNSSK ",ay-1)),col=c("black","grey50"),pch=16,lty=1)
dev.off()

# Catch
taf.png(paste0("output/SAM/catch.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
catchplot(fit, las = 0, xlab = "", main="Catch")
dev.off()


# leave-one-out plots ---------------------------------------------------

names(LO) <- paste("w.o.", c("NS-WC Q1", "NS-WC Q3+Q4"))

taf.png(paste0("output/SAM/leaveout_SSB.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
ssbplot(LO, las = 0, ci = TRUE, xlab = "", main="SSB")

dev.off()

taf.png(paste0("output/SAM/leaveout_Fbar.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
fbarplot(LO, ci = TRUE, partial = FALSE, las = 0, xlab = "", main="Fbar (2-4)")

dev.off()

taf.png(paste0("output/SAM/leaveout_Rec.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
recplot(LO, ci = TRUE, las = 0, xlab = "", main="Rec (age 0)")

dev.off()

taf.png(paste0("output/SAM/leaveout_Catch.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
catchplot(LO, ci = TRUE, las = 0, xlab = "", obs.show = FALSE,main="Catch")
dev.off()

# retrospective plots ---------------------------------------------------

#calc Mohns rho
mr.0 <- stockassessment::mohn(RETRO,lag=0) # for SSB
mr.1 <- stockassessment::mohn(RETRO,lag=1) # for  F and Rec
cm <- stockassessment::mohn(RETRO, lag=1, catchtable)[1]

taf.png(paste0("output/SAM/retro_SSB.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
ssbplot(RETRO, las = 0,  ci = TRUE, xlab = "",main=paste0("Mohn's rho = ",icesRound(mr.0["SSB"])))
dev.off()

taf.png(paste0("output/SAM/retro_Fbar.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
fbarplot(RETRO, ci = TRUE,drop=1, partial = FALSE, las = 0, xlab = "",main=paste0("Mohn's rho = ",icesRound(mr.1["Fbar(2-4)"])))
dev.off()

taf.png(paste0("output/SAM/retro_rec.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
recplot(RETRO, ci = TRUE, las = 0,drop=1, xlab = "",main=paste0("Mohn's rho = ",icesRound(mr.1["R(age 0)"])))
dev.off()

taf.png(paste0("output/SAM/retro_catch.png"), width = WIDTH+2, height = HEIGHT+1, units = UNITS, res = RESO)
par(mar = MAR, mgp = MGP, ps = PS)
catchplot(RETRO, ci = TRUE, las = 0, drop=1, xlab = "", obs.show = FALSE,main=paste0("Mohn's rho = ",icesRound(cm)))
dev.off()


# residuals plots ------------------------------------------------------------

attr(RESP, 'fleetNames')[[2]]<- c("Joint sample residuals log(F)")

taf.png(paste0("output/SAM/residuals.png"), width = 2000, height = 2000)
par(ps = PS)
plot(RES)
dev.off()

taf.png(paste0("output/SAM/procres.png"), width = 2000, height = 2000)
par(ps = PS)
plot(RESP)
dev.off()

# OSA residuals versus predicted value (for catch and for surveys).
# is there a trend? If so then might need pred-var link?

# Test residuals for normality (shapiro.test in R). (Shapiro and Wilk’s W test see P. Royston [4] for further details on the implementation).

# other diagnostic plots ----------------------------------------------------

# fit to observations
for(f in 1:fit$data$noFleets){
  taf.png(paste0(output.dir,"fleet", f,".png"),
          width = 1600, height = 1200, pointsize = PS)
  fitplot(fit, fleets=f)
  dev.off()
}

# sd plot
png(paste0("output/SAM/sd plot.png"),width = 10, height = 7, units = "in", res = 600)
sdplot(fit)
dev.off()

# corrected sd plot!!
sdSeries <- matrix(exp(fit$pl$logSdLogObs),
                   nrow = 1)

colnames(sdSeries) <- c("Residual catch age 0", "Residual catch age 1", "Residual catch age 2-8+", 
                        "deltaGAM NS-WC Q1 age 1", "deltaGAM NS-WC Q1 age 2-3","deltaGAM NS-WC Q1 age 4-6",
                        "deltaGAM NS-WC Q1 age 7-8+",
                        "deltaGAM NS-WC Q3+Q4 age 0","deltaGAM NS-WC Q3+Q4 age 1","deltaGAM NS-WC Q3+Q4 age 2",
                        "deltaGAM NS-WC Q3+Q4 age 3-5","deltaGAM NS-WC Q3+Q4 age 6-8+")
row.names(sdSeries) <- "sd"

## Extraction weights (might need to subset ages if uncoupled SDs):
Wi1 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 2 & fit$data$aux[ , "age"] == 1]
Wi2 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 2 & fit$data$aux[ , "age"] %in% 2:3]
Wi3 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 2 & fit$data$aux[ , "age"] %in% 4:6]
Wi4 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 2 & fit$data$aux[ , "age"] >6]

## correct SD according to Anders Nielsen: sqrt(mean(exp(2 * logSd) * (1/W))):
sdSeries[4] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[4]) * (1/Wi1), na.rm = TRUE))
sdSeries[5] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[5]) * (1/Wi2), na.rm = TRUE))
sdSeries[6] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[6]) * (1/Wi3), na.rm = TRUE))
sdSeries[7] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[7]) * (1/Wi4), na.rm = TRUE))


## Extraction weights (might need to subset ages if uncoupled SDs):
Wi1 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 3 & fit$data$aux[ , "age"] == 0]
Wi2 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 3 & fit$data$aux[ , "age"] == 1]
Wi3 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 3 & fit$data$aux[ , "age"] == 2]
Wi4 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 3 & fit$data$aux[ , "age"] %in% 3:5]
Wi5 <- fit$data$weight[fit$data$aux[ , "fleet"] %in% 3 & fit$data$aux[ , "age"] >5]

## correct SD according to Anders Nielsen: sqrt(mean(exp(2 * logSd) * (1/W))):
sdSeries[8] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[8]) * (1/Wi1), na.rm = TRUE))
sdSeries[9] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[9]) * (1/Wi2), na.rm = TRUE))
sdSeries[10] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[10]) * (1/Wi3), na.rm = TRUE))
sdSeries[11] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[11]) * (1/Wi4), na.rm = TRUE))
sdSeries[12] <- sqrt(mean(exp(2 * fit$pl$logSdLogObs[12]) * (1/Wi5), na.rm = TRUE))


sdSeries <- as.data.frame(sdSeries)
print(sdSeries)


# plot
tmp <-reshape2::melt(sdSeries)
tmp$Type <-  c(rep("Residual catch",3),
                       rep("deltaGAM NS-WC Q1",4),
                        rep("deltaGAM NS-WC Q3+Q4",5))

png(paste0("output/SAM/sd plot - corrected.png"),width = 10, height = 7, units = "in", res = 600)
p1 <- ggplot(tmp,aes(x=reorder(variable,value),y=value,col=Type,fill=Type))+
  geom_col()+theme_bw()+labs(x="",y="SD",col="",fill="")+
  scale_colour_manual(values=col.pal9)+scale_fill_manual(values=col.pal9)+
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))
print(p1)
dev.off()

# observed vs estimated catches
est.ca <- getFleet(fit,1,pred=T)
obs.ca <- getFleet(fit,1,pred=F)

est.ca <- reshape2::melt(est.ca)
est.ca$type = "Estimated"
obs.ca <- reshape2::melt(obs.ca)
obs.ca$type = "Observed"

catches <- rbind(est.ca,obs.ca)
names(catches) <- c("Year","Age","catch","type")

png(paste0("output/SAM/Observed vs estimated catches.png"),width = 10, height = 7, units = "in", res = 600)
p1 <- ggplot(catches,aes(x=Year,y=log(catch),group=type,colour=type)) + 
  geom_line()  +  facet_wrap(~Age)+
  labs(y="Catch numbers",col="",x="") +
  theme_bw() + theme(axis.title=element_text(size=7),
                     axis.text=element_text(size=6.5),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5),
                     legend.text=element_text(size=7))

print(p1)
dev.off()

# observed vs estimated catches for Q1 survey
est.ca <- getFleet(fit,2,pred=T)
obs.ca <- getFleet(fit,2,pred=F)

est.ca <- reshape2::melt(est.ca)
est.ca$type = "Estimated"
obs.ca <- reshape2::melt(obs.ca)
obs.ca$type = "Observed"

catches <- rbind(est.ca,obs.ca)
names(catches) <- c("Year","Age","catch","type")


png(paste0("output/SAM/Observed vs estimated NC-WC Q1 survey - log scale.png"),width = 10, height = 7, units = "in", res = 600)
p1 <- ggplot(catches,aes(x=Year,y=log(catch),group=type,colour=type)) + 
  geom_line()  +  facet_wrap(~Age)+
  labs(y="Catch numbers",col="",x="") +
  theme_bw() + theme(axis.title=element_text(size=7),
                     axis.text=element_text(size=6.5),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5),
                     legend.text=element_text(size=7))
print(p1)
dev.off()

# observed vs estimated catches for Q3+Q4 survey
est.ca <- getFleet(fit,3,pred=T)
obs.ca <- getFleet(fit,3,pred=F)

est.ca <- reshape2::melt(est.ca)
est.ca$type = "Estimated"
obs.ca <- reshape2::melt(obs.ca)
obs.ca$type = "Observed"

catches <- rbind(est.ca,obs.ca)
names(catches) <- c("Year","Age","catch","type")


png(paste0("output/SAM/Observed vs estimated NC-WC Q3Q4 survey - log scale.png"),width = 10, height = 7, units = "in", res = 600)
p1 <- ggplot(catches,aes(x=Year,y=log(catch),group=type,colour=type)) + 
  geom_line()  +  facet_wrap(~Age)+
  labs(y="Catch numbers",col="",x="") +
  theme_bw() + theme(axis.title=element_text(size=7),
                     axis.text=element_text(size=6.5),axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5),
                     legend.text=element_text(size=7))
print(p1)
dev.off()

png(paste0("output/SAM/Survey catchabilities.png"),width = 10, height = 7, units = "in", res = 600)

qtableplot(qtable(fit))
dev.off()

# percent of stock by year class (percentage bar with colour by age or cohort?)
# like iceland haddock
# summarise age by year 

dat <- as.data.frame(ntable(fit))
dat$Year <- row.names(dat)
age.dist <- pivot_longer(dat,cols=as.character(0:8),names_to="Age",values_to="num")
age.dist$Age <- as.numeric(age.dist$Age)
age.dist$Year <- as.numeric(as.character(age.dist$Year))

age.dist$cohort <- as.character(age.dist$Year-age.dist$Age)
age.dist$Age <- as.factor(age.dist$Age)

# attribute cohort colours
repn <- ceiling(length(unique(age.dist$cohort))/length(col.pal9))
col.pal <- rep(col.pal9,repn)[1:length(unique(age.dist$cohort))]
cohortLst <- sort(unique(age.dist$cohort))
age.dist$col <- col.pal[match(age.dist$cohort,cohortLst)]

png(filename="output/SAM/Age distribution by cohort.png", height=7, width=11, , units = "in", res = 300)

age.dist <- filter(age.dist,Year<ay)
p1 <- ggplot(age.dist,aes(x=Year,y=num,fill=cohort))+
  geom_col(position="fill")+theme_bw()+labs(x="",y="Stock number proportion")+
  scale_fill_manual(values = age.dist$col)+
  theme_bw()+theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1))
print(p1)
dev.off()


## proportion ssb
n. <- ntable(fit)
mat <- fit$data$propMat
stk.wts <- fit$data$stockMeanWeight

dat <- as.data.frame(n.*mat*stk.wts)

dat$Year <- row.names(dat)
age.dist <- pivot_longer(dat,cols=as.character(0:8),names_to="Age",values_to="num")
age.dist$Age <- as.numeric(age.dist$Age)
age.dist$Year <- as.numeric(as.character(age.dist$Year))

age.dist$cohort <- as.character(age.dist$Year-age.dist$Age)
age.dist$Age <- as.factor(age.dist$Age)

# attribute cohort colours
repn <- ceiling(length(unique(age.dist$cohort))/length(col.pal9))
col.pal <- rep(col.pal9,repn)[1:length(unique(age.dist$cohort))]
cohortLst <- sort(unique(age.dist$cohort))
age.dist$col <- col.pal[match(age.dist$cohort,cohortLst)]

png(filename="output/SAM/SSB distribution by cohort.png", height=7, width=11, , units = "in", res = 300)

#age.dist <- filter(age.dist,Year<ay)
p1 <- ggplot(filter(age.dist,Year<ay),aes(x=Year,y=num,fill=cohort))+
  geom_col(position="fill")+theme_bw()+labs(x="",y="SSB proportion")+
  scale_fill_manual(values = age.dist$col)+
  theme_bw()+theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1))
print(p1)
dev.off()

png(filename="output/SAM/SSB contribution by cohort.png", height=7, width=11, , units = "in", res = 300)

#age.dist <- filter(age.dist,Year<ay)
p1 <- ggplot(age.dist,aes(x=Year,y=num,fill=cohort))+
  geom_col()+theme_bw()+labs(x="",y="SSB proportion")+
  scale_fill_manual(values = age.dist$col)+
  theme_bw()+theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1))
print(p1)
dev.off()

# Proportion of each age in SSB

ssb <- ssbtable(fit)[,1]
tab_numbers <- ntable(fit)
mat <- fit$data$propMat
stk.wts <- fit$data$stockMeanWeight

bio_at_age <- tab_numbers*mat*stk.wts
ssb.m <- bio_at_age*NA
for (y in 1972:ay){
  ssb.m[ac(y),] <- ssb[ac(y)]
}

dat <- reshape2::melt(bio_at_age/ssb.m)
names(dat) <- c("Year","age","propSSB")

png(paste0("output/SAM/Proportion of SSB at age by year.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(Year, propSSB,group=age)) +
  facet_wrap(~age)+ ylim(c(0,1))+
  geom_line() + theme_bw()+ labs(y="Proportion of SSB",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

png(paste0("output/SAM/Proportion of SSB each year by age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat[dat$Year >=1999,], aes(age, propSSB,group=Year)) +
  facet_wrap(~Year)+ ylim(c(0,1))+
  geom_line() + theme_bw()+ labs(y="Proportion of SSB",x="") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

png(paste0("output/SAM/Proportion of SSB for plus group.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat[dat$age==8,], aes(Year, propSSB,group=age)) +
  geom_line() + theme_bw()+ labs(y="Proportion of SSB",x="") +
  theme(axis.title=element_text(size=10),axis.text=element_text(size=10),
        legend.text=element_text(size=11)) 
print(p1)
dev.off()


# make comparison plot - SURBAR, SAM -----------------------------------


ts_yrs_sbr <- 1983:ay

#get natural mortality

nm<-readVPAFile("data/SAM/nm.dat")
nm<-nm[ac(0:8),ac(ts_yrs_sbr)]
nm <- t(matrix(nm,ncol=length(ts_yrs_sbr),nrow=length(0:8),dimnames=list(0:8,ts_yrs_sbr)))

### load SAM and SURBAR results 

# SAM
sam_res<-read.csv("output/SAM/tab_summary.csv")
names(sam_res)<-c("Year","Rage0","RLow","RHigh","SSB","SLow","SHigh","Fbar","FLow","FHigh","TSB","TLow","THigh")

sam_n<-read.csv("output/SAM/tab_numbers.csv")
sam_f<-read.csv("output/SAM/tab_fay.csv")
names(sam_n)<-names(sam_f)<-c("Year",as.character(0:8))

# SURBAR
sbr_res<-read.csv("output/SURBAR/SURBAR stock summary results.csv")

### Mean-std estimate
df<-data.frame(Year=ts_yrs_sbr,
               SAM_SSB=NA,SAM_N1=NA,SAM_meanZ=NA,
               SBR_SSB=NA,SBR_N1=NA,SBR_meanZ=NA)

# adjust time series
sam_res<-sam_res[sam_res$Year %in% ts_yrs_sbr,]
sam_n<-sam_n[sam_n$Year %in% ts_yrs_sbr,]
sam_f<-sam_f[sam_f$Year %in% ts_yrs_sbr,]
sbr_res<-sbr_res[sbr_res$year %in% ts_yrs_sbr,]

#SSB
df$SAM_SSB<-sam_res$SSB/mean(sam_res$SSB)
df$SBR_SSB<-sbr_res$ssb/mean(sbr_res$ssb)

#Rec age 1
df$SAM_N1<-sam_n[,as.character(1)]/mean(sam_n[,as.character(1)])
df$SBR_N1<-sbr_res$rec/mean(sbr_res$rec)

#mean Z
samZ <- rowMeans((sam_f[,2:10]+nm)[,as.character(2:4)])
df$SAM_meanZ<-samZ/mean(samZ)
df$SBR_meanZ<-sbr_res$meanz/mean(sbr_res$meanz)

# remove last data point for Z
df$SAM_meanZ[df$Year>(ay-1)] <- NA
df$SBR_meanZ[df$Year>(ay-1)] <- NA

### plot 
windows(height=7,width=11)
layout(rbind(c(1,2),c(3,4)))

# SSB
ylims<-range(c(df$SAM_SSB,df$SBR_SSB))
plot(ts_yrs_sbr,df$SAM_SSB,col="blue",type="l",lwd=2,ylim=ylims,xlab="Year",ylab="Mean-std SSB",main="SSB")
lines(ts_yrs_sbr,df$SBR_SSB,col="grey20",type="l",lwd=2)
legend("topleft",inset=0.02,legend=c("SAM","SURBAR"),lwd=2,col=c("blue","grey20"),lty=1,cex=0.8)

#Rec
ylims<-range(c(df$SAM_N1,df$SBR_N1))
plot(ts_yrs_sbr,df$SAM_N1,col="blue",type="l",lwd=2,ylim=ylims,xlab="Year",ylab="Mean-std Rec (age 1)",main="Recruitment (age 1)")
lines(ts_yrs_sbr,df$SBR_N1,col="grey20",type="l",lwd=2)
legend("topright",inset=0.02,legend=c("SAM","SURBAR"),lwd=2,col=c("blue","grey20"),lty=1,cex=0.8)

#meanZ
ylims<-range(c(df$SAM_meanZ,df$SBR_meanZ),na.rm=T)
plot(ts_yrs_sbr,df$SAM_meanZ,col="blue",type="l",lwd=2,ylim=ylims,xlab="Year",ylab="Mean-std Z (2-4)",main="Mean Z (2-4)")
lines(ts_yrs_sbr,df$SBR_meanZ,col="grey20",type="l",lwd=2)
legend("topright",inset=0.02,legend=c("SAM","SURBAR"),lwd=2,col=c("blue","grey20"),lty=1,cex=0.8)

savePlot(filename = "output/SURBAR/Stock summary SAM vs SURBAR",type = "png")


# make SAM fit comparison plots ----------------------------------------------------------------

### this year vs last year's assessment

taf.png(paste0("output/SAM/Stock summary WGNSSK ",ay," vs WGNSSK ",(ay-1)), width = 11, height = 7, units = UNITS, res = RESO)
fits <- c(WGNSSK_2024 = prev_ass,WGNSSK_2025 = fit)
par(mar = MAR, mgp = MGP, ps = PS)
plot(fits, partial = FALSE, las = 0, xlab = "",addCI=TRUE)
dev.off()

taf.png(paste0("output/SAM/Recruitment WGNSSK ",ay," vs WGNSSK ",(ay-1)), width = 11, height = 7, units = UNITS, res = RESO)
#fits <- c(WGNSSK_2023 = prev_ass,WGNSSK_2024 = fit)
par(mar = MAR, mgp = MGP, ps = PS)
recplot(fit, las = 0, xlab = "",addCI=TRUE,drop=T)
recplot(prev_ass, las = 0, xlab = "",addCI=TRUE,add=T,cicol=alpha("lightblue",alpha=0.6))
legend("topright",inset=0.02,legend=c(paste0("WGNSSK ",ay),paste0("WGNSSK ",ay-1)),col="black",lty=c(1,3),lwd=2,fill=c(alpha("grey30",0.5),
                                                                                                                     alpha("lightblue",0.6)))
dev.off()

# Extra plots ---------------------------------------------------------

# stock numbers and total biomass

tab_numbers <- ntable(fit)
stk.wts <- fit$data$stockMeanWeight
mat <- fit$data$propMat

total_biomass <- as.data.frame(tab_numbers*stk.wts)
stockn <- as.data.frame(tab_numbers)
ssb <- as.data.frame(tab_numbers*stk.wts*mat)

total_biomass$Year <- 1972:2025
stockn$Year <- 1972:2025
ssb$Year <- 1972:2025

dat1 <- pivot_longer(total_biomass,cols=c(as.character(0:8)),names_to="Age",values_to="biomass")
dat2 <- pivot_longer(stockn,cols=c(as.character(0:8)),names_to="Age",values_to="number")
dat3 <- pivot_longer(ssb,cols=c(as.character(0:8)),names_to="Age",values_to="biomass")

dat1$label <- "Total biomass"
dat3$label <- "SSB"

dat4 <- bind_rows(dat1,dat3)

dat <- dat2 %>% group_by(Year) %>% summarise(total=sum(number))
p1 <- ggplot(dat,aes(x=Year,y=total))+geom_line()+geom_point()+theme_bw()+labs(x="",y="Stock numbers")

dat <- dat4 %>% group_by(Year,label) %>% summarise(total=sum(biomass))
p3 <- ggplot(dat,aes(x=Year,y=total,colour=label))+geom_line()+geom_point()+theme_bw()+
  labs(x="",y="biomass",colour="")+theme(legend.position = "bottom")


taf.png(paste0("output/SAM/Total biomass and stock numbers.png"), width = 11, height = 7, units = UNITS, res = RESO)

plot_grid(p1,p3,nrow=1,align="hv")
dev.off()


# change in 2023 and 2024
tmp <- filter(dat3, Year >2022 & Year < 2025)
tmp <- pivot_wider(tmp,id_cols=c("Age","label"),names_from = "Year",values_from="biomass")
tmp$diff <- tmp$`2024`-tmp$`2023`
tmp$`2023` <- 0
tmp$`2024` <- tmp$diff
tmp <- tmp[ -5]
tmp <- pivot_longer(tmp,cols=c("2023","2024"),names_to="Year",values_to="diff")

taf.png(paste0("output/SAM/SSB diff from 2023 by age.png"), width = 11, height = 7, units = UNITS, res = RESO)

p1 <- ggplot(tmp,aes(x=as.numeric(Year),y=diff,colour=Age))+geom_line()+theme_bw()+labs(x="",y="SSB difference to 2023",colour="")+
  scale_colour_manual(values = col.pal9)
print(p1)
dev.off()




