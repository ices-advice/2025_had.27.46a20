
# Make csv outputs for chagne in advice Rmarkdown

graphics.off()

# Inputs:
# Forecast_assumptions.csv - table of recruitment, SSB, Fbar and total catch from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_stockwts.csv - table of stock weights at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_selectivity.csv - table of selectivity at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Forecast_N_at_age.csv - table of stock numbers at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Compare_forecast_B_at_age.csv - table of stock biomass at age used in the forecast from this year's WG and last year's WG for the data year, intermediate year and advice year.
# Now_assessment_N_at_age.csv - table of stock numbers at age from THIS year's assessment. Usually a standard output for the WG report.
# Now_assessment_B_at_age.csv - table of stock biomass at age from THIS year's assessment. Multiply "now_assessment_N_at_age.csv" by stock weights at age
# Prev_assessment_N_at_age.csv - table of stock numbers at age from LAST year's assessment. Usually a standard output for the WG report.
# Prev_assessment_B_at_age.csv - table of stock biomass at age from LAST year's assessment. Multiply "now_assessment_N_at_age.csv" by stock weights at age


# Need to think about if I should use median value for Rec or sub in geometric mean value??

# settings ----------------------------------------
output.dir <- "output/Change_in_advice/"

data_yrs <- 1972:(ay-1)

## Forecast parameters:
Ay <- (ay-3):(ay-1) # for biols
Sy <- (ay-3):(ay-1)  # for sel
Ry <- 2000:(ay-1) # for rec

ac<-as.character

# load assessments and forecasts -------------------------------------
load("model/SAM/NShaddock_WGNSSK2024_Run2/model.RData") # last year's
prev_ass <- fit
load("boot/data/forecast - WGNSSK 2024.RData")
prev_FC1 <- FC1
prev_frcst_fit <- attr(prev_FC1, "fit")

# load this year's assessment and forecast
load("model/SAM/NShaddock_WGNSSK2025_Run1/model.RData")

load("model/SAM/forecast.RData")

# Forecast_assumptions.csv  -----------------------------------------------

# this year
tab1 <- data.frame(WG =paste0("WGNSSK ",ay),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-1):(ay+1),4), Type=NA, Value= NA, Source = "Forecast")

tab1$Type[tab1$Year %in% (ay-1)] <- "Data year"
tab1$Type[tab1$Year %in% (ay)] <- "Intermediate year"
tab1$Type[tab1$Year %in% (ay+1)] <- "Advice year"

tab1$Source[tab1$Year %in% (ay-1)] <- "Assessment"

astab <- as.data.frame(summary(fit))
astab$Year <- rownames(astab)
tmp_tab <- catchtable(fit)
fctab <- attr(FC[[1]],"tab")

# data year values
idx <- which(astab$Year %in% c(ay-1)) # data yr
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Recruitment"] <- astab$"R(age 0)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Fbar"] <- astab$"Fbar(2-4)"[idx] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Total catch"] <- catchtable(fit)[as.character(ay-1),"Estimate"] # data year

# int year values
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]

# advice year values
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay+1),"ssb:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay+1),"fbar:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay+1),"catch:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay+1),"rec:median"]

Ry <- 2000:(ay-1)
R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even

tab1$Value[tab1$WG %in% paste0("WGNSSK ",ay) & tab1$Variable %in% "Recruitment" & !tab1$Source %in% "Assessment"] <- R_geoMean


# last year
tab2 <- data.frame(WG =paste0("WGNSSK ",ay-1),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-2):(ay),4), Type=NA, Value= NA, Source = "Forecast")

tab2$Type[tab2$Year %in% (ay-2)] <- "Data year"
tab2$Type[tab2$Year %in% (ay-1)] <- "Intermediate year"
tab2$Type[tab2$Year %in% (ay)] <- "Advice year"

tab2$Source[tab2$Year %in% (ay-2)] <- "Assessment"

astab <- getSAG(stock = "had.27.46a20",year=(ay-1),purpose="Advice")
fctab <- attr(prev_FC1,"tab")

# data year values
idx <- which(astab$Year %in% c(ay-2)) # data yr
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Recruitment"] <- astab$recruitment[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Fbar"] <- astab$"F"[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Total catch"] <- astab$catches[idx] # data year

# int year values
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay-1),"ssb:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay-1),"fbar:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay-1),"catch:median"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Recruitment"] <- fctab[as.character(ay-1),"rec:median"]

# advice year values
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]


# combine tables and export
tab <- rbind(tab1,tab2)

write.csv(tab,paste0(output.dir,"Forecast_assumptions.csv"),row.names=F)


# Forecast_stockwts.csv ---------------------------------------------------

# this year
ca.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - catch-at-age.txt")
st.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - stock-at-age.txt")
lan.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - landings-at-age.txt")
dis.frct.wt <- read.table("output/Forecast/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt")

colnames(ca.frct.wt) <- colnames(st.frct.wt) <- colnames(lan.frct.wt) <- colnames(dis.frct.wt) <- colnames(fit$data$catchMeanWeight)


fit$data$catchMeanWeight[,,"Residual catch"] <- rbind(fit$data$catchMeanWeight[ac(data_yrs),,"Residual catch"],ca.frct.wt)
fit$data$stockMeanWeight <- rbind(fit$data$stockMeanWeight[ac(data_yrs),],st.frct.wt) 
fit$data$landMeanWeight[,,"Residual catch"] <- rbind(fit$data$landMeanWeight[ac(data_yrs),,"Residual catch"],lan.frct.wt)
fit$data$disMeanWeight[,,"Residual catch"] <- rbind(fit$data$disMeanWeight[ac(data_yrs),,"Residual catch"],dis.frct.wt) 

ca.frct.wt$Year <- st.frct.wt$Year <- lan.frct.wt$Year <- dis.frct.wt$Year <- row.names(ca.frct.wt)
ca.frct.wt$Cat <- "catch"
st.frct.wt$Cat <- "stock"
lan.frct.wt$Cat <- "landings"
dis.frct.wt$Cat <- "discards"

dat <- reshape2::melt(rbind(ca.frct.wt,st.frct.wt,lan.frct.wt,dis.frct.wt),id.vars=c("Year","Cat"))
names(dat) <- c("Year","Cat","age","wt")

# age 0 - no observations - set to NA
dat[dat$age==0 & dat$wt==0,"wt"]<-NA

dat <- dat[dat$Cat %in% "stock",]
dat$Source <- paste0("WGNSSK ",ay)

# compare to last year's stock weights
prev_frcst.wt <- prev_frcst_fit$data$stockMeanWeight
prev_frcst.wt$Year <- rownames(prev_frcst.wt)
prev_frcst.wt$Cat <- "stock"
prev_frcst.wt$Source <- paste0("WGNSSK ",(ay-1))
dat.prev <- reshape2::melt(prev_frcst.wt,id.vars=c("Source","Cat","Year"))
names(dat.prev) <- c("Source","Cat","Year","age","wt")
dat.prev <- dat.prev[dat.prev$Year %in% ((ay-1):(ay+1)),]

dat <- rbind(dat,dat.prev)

png(paste0(output.dir,"Compare inputs - stock mean weights.png"),width = 11, height = 5, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=age, y=wt,colour=Source,group=Source)) + 
  facet_wrap(~Year,nrow = 2)+ geom_point()+
  geom_line() + theme_bw()+ labs(colour="",x="",y="mean weight (kg)") +
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# save out
colnames(dat) <- c("Year","Cat","Age","Weight","Source")

write.csv(dat[,c("Year","Age","Weight","Source")],file = paste0(output.dir,"Forecast_stockwts.csv"),row.names=F)

# Forecast_selectivity.csv --------------------------------------------------

Fsel <- faytable(fit)/rowSums(faytable(fit))
Fsel.frcst <- colMeans(Fsel[ac(Sy),])

# last year's sel
Fsel <- faytable(prev_ass)/rowSums(faytable(prev_ass))
Fsel.prev <- colMeans(Fsel[ac(Sy-1),])

png(paste0(output.dir,"Compare inputs - selectivity.png"),width = 11, height = 7, units = "in", res = 600)

plot(0:8,Fsel.frcst,pch=16,ylab="Selectivity",xlab="Age")
lines(0:8,Fsel.frcst,lty=1)
points(0:8,Fsel.prev,pch=16)
lines(0:8,Fsel.prev,lty=3)

legend("bottomright", inset=0.02,legend=c(paste0("WGNSSK ",ay),paste0("WGNSSK ",(ay-1))),col="black",lty=c(1,3),pch=c(16))
dev.off()

# write out
dat1 <- reshape2::melt(Fsel.frcst)
dat1$Age <- rownames(dat1)
dat1$Source <- paste0("WGNSSK ",ay)

dat2 <- reshape2::melt(Fsel.prev)
dat2$Age <- rownames(dat2)
dat2$Source <- paste0("WGNSSK ",(ay-1))

dat <- rbind(dat1,dat2)
colnames(dat) <- gsub("value","Selectivity",colnames(dat))

write.csv(dat,file = paste0(output.dir,"Forecast_selectivity.csv"),row.names=F)


# N at age compared to previous forecast ------------------------------------------

# get forecast n at age tables
natage_now <- attr(FC[[1]],"naytable")
natage_prev <- attr(prev_FC1,"naytable")

n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(fit)))
colnames(n_now) <- c("Year",0:8)

n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)

n_now <- reshape2::melt(n_now,id.vars="Year")
n_prev <- reshape2::melt(n_prev,id.vars="Year")

dat0 <- n_now[n_now$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- n_now[n_now$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- n_prev[n_prev$Year == (ay-2),]
dat2$WG <- paste0("WGNSSK ",(ay-1))
dat2$Type="Data"

dat3 <- data.frame(value=natage_now[4,],Year=ay,variable=0:8) # 4th row is int yr
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- data.frame(value=natage_prev[4,],Year=ay-1,variable=0:8) # 4th row is int yr
dat4$WG <- paste0("WGNSSK ",(ay-1))
dat4$Type="Intermediate year"

dat5 <- data.frame(value=natage_now[7,],Year=ay+1,variable=0:8) # 7th row is advice yr
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <-  data.frame(value=natage_prev[7,],Year=ay,variable=0:8) # 7th row is advice yr
dat6$WG <- paste0("WGNSSK ",(ay-1))
dat6$Type="Advice year"


dat <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(dat) <- c("Year","Age","N","WG","Type")
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

# correct Rec from WGNSSK ay to account for geometric mean
Ry <- 2000:(ay-1)
R <- rectable(fit)[,1]
R <- R[as.character(1972:(ay-1))]
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even

dat$N[dat$WG %in% paste0("WGNSSK ",ay) & dat$Age == 0 & !dat$Type %in% "Data"] <- R_geoMean


write.csv(dat,file=paste0(output.dir,"Forecast_N_at_age.csv"),row.names=F)

png(paste0(output.dir,"Compare inputs - Stock numbers-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Abundance (thousands)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))

print(p1)
dev.off()


png(paste0(output.dir,"Compare inputs - Stock numbers-at-age_v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=as.factor(Year)))+ geom_line()+geom_point(size=3)+
  labs(colour="",y="Abundance (thousands)",shape="")+
  facet_wrap(~Type)+theme_bw()

p2 <- ggplot(dat,aes(x=Age,y=N,group=interaction(Year,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+
  labs(colour="",y="Abundance (thousands)",shape="")+
  facet_wrap(~Year,nrow=1)+theme_bw()

plot_grid(p1,p2,nrow=2)
dev.off()

res.n <- res.bm <- vector(mode="list",length=length(ts_yrs))
names(res.n) <- names(res.bm) <- ts_yrs

# compare N at age table ----------------------------------

# this year's assessment and forecast
n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(fit)))
colnames(n_now) <- c("Year",0:8)
n_now <- n_now[n_now$Year<ay,] # remove int year values

natage_now <- as.data.frame(attr(FC[[1]],"naytable")[c(4,7,10),])
natage_now$Year <- ay:(ay+2)
colnames(natage_now) <- c(0:8,"Year")

n_now <- rbind(n_now,natage_now)
row.names(n_now) <- n_now$Year

# last year's assessment and forecast
n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)
n_prev <- n_prev[n_prev$Year<(ay-1),] # remove int year values

natage_prev <- as.data.frame(attr(prev_FC1,"naytable")[c(4,7,10),])
natage_prev$Year <- (ay-1):(ay+1)
colnames(natage_prev) <- c(0:8,"Year")

n_prev <- rbind(n_prev,natage_prev)
row.names(n_prev) <- n_prev$Year

# correct Rec from WGNSSK 2023 to account for geometric mean
#n_prev[n_prev$Year %in% c(2023:2025),"0"] <- 1608819

# save results
write.csv(n_now,file=paste0(output.dir,"Now_assessment_N_at_age.csv"),row.names=F)
write.csv(n_prev,file=paste0(output.dir,"Prev_assessment_N_at_age.csv"),row.names=F)


# biomass at age compared to previous forecast -----------------------------------------

# get forecast n at age tables
natage_now <- attr(FC[[1]],"naytable")
wtatage_now <- as.data.frame(attr(FC[[1]],"fit")$data$stockMeanWeight[as.character(ay:(ay+1)),])
natage_prev <- attr(prev_FC1,"naytable")
# corrction for geometric mean at WGNSSK 2022
#natage_prev[c(4,7),1] <- 1608819
wtatage_prev <- as.data.frame(prev_frcst_fit$data$stockMeanWeight[as.character((ay-1):ay),])
# 
n_now <- as.data.frame(cbind(ts_yrs,ntable(fit)))
colnames(n_now) <- c("Year",0:8)
wt_now <- as.data.frame(attr(FC[[1]],"fit")$data$stockMeanWeight[as.character(1972:ay),])
b_now <- n_now[,-1]*wt_now
b_now$Year <- n_now$Year

n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)
wt_prev <- as.data.frame(prev_frcst_fit$data$stockMeanWeight[as.character(1972:(ay-1)),])
b_prev <- n_prev[,-1]*wt_prev
b_prev$Year <- n_prev$Year

b_now <- reshape2::melt(b_now,id.vars="Year")
b_prev <- reshape2::melt(b_prev,id.vars="Year")

dat0 <- b_now[b_now$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- b_now[b_now$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- b_prev[b_prev$Year == (ay-2),]
dat2$WG <- paste0("WGNSSK ",(ay-1))
dat2$Type="Data"

dat3 <- data.frame(value=natage_now[4,]*as.numeric(wtatage_now[as.character(ay),]),Year=ay,variable=0:8) # 4th row is int yr
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- data.frame(value=natage_prev[4,]*as.numeric(wtatage_prev[as.character(ay-1),]),Year=ay-1,variable=0:8) # 4th row is int yr
dat4$WG <- paste0("WGNSSK ",(ay-1))
dat4$Type="Intermediate year"

dat5 <- data.frame(value=natage_now[7,]*as.numeric(wtatage_now[as.character(ay+1),]),Year=ay+1,variable=0:8) # 7th row is advice yr
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <-  data.frame(value=natage_prev[7,]*as.numeric(wtatage_prev[as.character(ay),]),Year=ay,variable=0:8) # 7th row is advice yr
dat6$WG <- paste0("WGNSSK ",(ay-1))
dat6$Type="Advice year"

dat <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(dat) <- c("Year","Age","B","WG","Type")
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))


write.csv(dat,file=paste0(output.dir,"Forecast_B_at_age.csv"),row.names=F)


png(paste0("output/Forecast/Compare inputs - Stock biomass-at-age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))

print(p1)

dev.off()


png(paste0("output/Forecast/Compare inputs - Stock biomass-at-age_v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=Age,y=B,group=interaction(Year,WG),colour=WG,shape=as.factor(Year)))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Type)+theme_bw()

p2 <- ggplot(dat,aes(x=Age,y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Year,nrow=1)+theme_bw()

plot_grid(p1,p2,nrow=2)

dev.off()

# compare B at age table ----------------------------------

# this year's assessment and forecast
n_now <- as.data.frame(cbind(c(1972:(ay)),ntable(fit)))
colnames(n_now) <- c("Year",0:8)
n_now <- n_now[n_now$Year<ay,] # remove int year values

natage_now <- as.data.frame(attr(FC[[1]],"naytable")[c(4,7,10),])
natage_now$Year <- ay:(ay+2)
colnames(natage_now) <- c(0:8,"Year")

n_now <- rbind(n_now,natage_now)
row.names(n_now) <- n_now$Year

wtatage_now <- as.data.frame(attr(FC[[1]],"fit")$data$stockMeanWeight)
b_now <- n_now[,ac(0:8)]*wtatage_now
b_now$Year <- n_now$Year

# last year's assessment and forecast
n_prev <- as.data.frame(cbind(c(1972:(ay-1)),ntable(prev_ass)))
colnames(n_prev) <- c("Year",0:8)
n_prev <- n_prev[n_prev$Year<(ay-1),] # remove int year values

natage_prev <- as.data.frame(attr(prev_FC1,"naytable")[c(4,7,10),])
natage_prev$Year <- (ay-1):(ay+1)
colnames(natage_prev) <- c(0:8,"Year")

n_prev <- rbind(n_prev,natage_prev)
row.names(n_prev) <- n_prev$Year

# correct Rec from WGNSSK 2022 to account for geometric mean
#n_prev[n_prev$Year %in% c(2022:2024),"0"] <- 1608819

wtatage_prev <- as.data.frame(prev_frcst_fit$data$stockMeanWeight)
b_prev <- n_prev[,ac(0:8)]*wtatage_prev
b_prev$Year <- n_prev$Year

b_now <- b_now[,c(10,1:9)]
b_prev <- b_prev[,c(10,1:9)]

# save results
write.csv(b_now,file=paste0(output.dir,"Now_assessment_B_at_age.csv"),row.names=F)
write.csv(b_prev,file=paste0(output.dir,"Prev_assessment_B_at_age.csv"),row.names=F)

