##

## plot up input data - plots for report

load("data/init.RData")

output.dir <- "output/input data/"
output.flr <- paste0("data/FLR files - WGNSSK ",ay,"/") 

# load stock data
load("data/stockData.RData")
load("data/indices.RData")

textSize <- theme(axis.title=element_text(size=12),axis.text=element_text(size=12),
                  legend.text=element_text(size=12),strip.text = element_text(size=12)) 

# Maturity -----------------------------------------------------------
input.flr <- paste0("boot/data/FLR files - WGNSSK ",ay-1,"/") # last year's FLR files
mo_old <- readVPAFile(paste0(input.flr,"/nor_had_mo.txt"))
mo_old <- window(trim(mo_old,age=0:8),start=1972)

# plot new ogives
mo_new <- read.csv("data/Maturity/had.27.46a20 - Maturity ogive.csv")
names(mo_new) <- c("Year",0:8)
mo_new <- FLQuant(t(mo_new[,-1]),dimnames=list(age=0:8,year=1965:ay))
mo_new <- window(trim(mo_new, age=0:8),start=1972)
mo_new[,ac(ay)] <- mo_new[,ac(ay-1)]

png(paste0(output.dir,fn.prefix," - Maturity ogive.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=FLQuants(WGNSSK_2025=mo_new,WGNSSK_2024=mo_old), aes(year, data)) + 
  geom_line(aes(colour=factor(age),linetype=qname)) + theme_bw()+ 
  labs(y="Proportion mature",colour="Age",x="",linetype="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()


# Natural mortality ----------------------------------------------

# check raw vs smoothed

nm <- read.csv(file="data/Natural mortality/had.27.46a20 - annual natural mortality from WGSAM 2023.csv")
nm_smoothed <- read.csv(file="data/Natural mortality/had.27.46a20 - annual smoothed natural mortality from WGSAM 2023.csv")

colnames(nm) <- gsub("X","",colnames(nm))
colnames(nm_smoothed) <- gsub("X","",colnames(nm_smoothed))

dat1 <- pivot_longer(nm,cols=c(as.character(0:10)),names_to="Age",values_to="M") 
dat1$Cat <- "raw"
dat2 <- pivot_longer(nm_smoothed,cols=c(as.character(0:10)),names_to="Age",values_to="M") 
dat2$Cat <- "smoothed"
dat <- bind_rows(dat1,dat2)
dat$Age <- as.numeric(dat$Age)

png(paste0(output.dir,"had.27.46a20 - Natural mortality SMS 2023 - raw vs smoothed.png"),width = 10, height = 7, units = "in", res = 600)
p1 <- ggplot(data=dat,aes(x=Year,y=M,group=Cat,colour=Cat)) +
  geom_line()+facet_wrap(~Age) +
  theme_bw()+ labs(y="Natural mortality",colour="",x="") +
  scale_colour_brewer(palette="Dark2") + textSize

print(p1)
dev.off()

# plot NM values for this year
nm <- readVPAFile(paste0(output.flr,"nor_had_nm.txt"))
nm <- trim(nm, age=0:8)

png(paste0(output.dir,fn.prefix," - Natural mortality.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=nm, aes(year, data)) + 
  geom_line(aes(colour=factor(age))) + theme_bw()+ labs(y="Natural mortality",colour="Age",x="") +
  scale_colour_manual(values=col.pal9) + textSize
print(p1)

dev.off()  

# Stock weights ------------------------------------------------------------------

# stock weights over time, all ages on same plot
png(paste0(output.dir,fn.prefix," - Stock weights.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(stock=stock.data.pg@stock.wt)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) +
  geom_line(linewidth=0.4,aes(group=age,colour=factor(age))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize
print(p1)
dev.off()

# Stock weights - facet by age
png(paste0(output.dir,fn.prefix," - Stock weights by age.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(stock=stock.data.pg@stock.wt)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) + 
  facet_wrap(~age,scales="free_y")+geom_line(linewidth=0.4,aes(group=age,colour=factor(age))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize
print(p1)
dev.off()

# Investigate density dependence

# calculate variables to plot
dat <- as.data.frame(datFL)
dat$year <- as.numeric(dat$year)
dat$age <- as.numeric(dat$age)

# get cohort
dat <- dat %>% rowwise() %>% mutate(cohort=year-age) 

# diff in mean weight by age
tmp <- dat %>% group_by(age) %>% summarise(mWt=mean(data))
dat <- left_join(dat,tmp)
dat <- dat %>% mutate(Wdiff=data-mWt)

# age labels
dat$age <- as.factor(dat$age)

# weight over time by age with smoother
toplot <- dat
p1 <- ggplot(toplot,aes(x=year, y=data,colour=age))+geom_point()+geom_smooth()+theme_bw()+
  scale_colour_manual(values=col.pal)+facet_wrap(~age,scales="free_y")+labs(x="",colour="",y="mean weight (g)")
png(file=paste0(output.dir,"Mean weights - stock wts mean weight over time by age.png"),height=7,width=11,res=200,units="in")
print(p1)
dev.off()


# iceland plot
toplot <- filter(dat,year>1999)
p1 <- ggplot(toplot,aes(x=year,y=Wdiff,colour=cohort,fill=cohort))+geom_col()+theme_bw()+
  #  scale_colour_manual(values=col.pal)+scale_fill_manual(values=col.pal)+
  facet_wrap(~age,scales="free_y",ncol=1,strip.position="right") + 
  labs(x="",y="Weight difference (g)")
png(file=paste0(output.dir,"Mean weights - mean weight difference over time by age.png"),height=11,width=7,res=200,units="in")
print(p1)
dev.off()


# Catch data --------------------------------------------------------

# yield
stock.data@catch <- computeCatch(stock.data)
stock.data@landings <- computeLandings(stock.data)
stock.data@discards <- computeDiscards(stock.data)

png(paste0(output.dir,fn.prefix," - Yield.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(catch=stock.data@catch, landings=stock.data@landings, discards=stock.data@discards,
                  BMS=quantSums(bmsn*bmsw),IBC=quantSums(ibcn*ibcw))

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  ylim(c(0,max(window(stock.data,start=1972)@catch,na.rm=T)))+
  geom_line(aes(group=qname,colour=factor(qname)))+ theme_bw()+ 
  labs(y="Yield (tonnes)",colour="",x="") +
  scale_colour_manual(values=col.pal)+textSize 
print(p1)
dev.off()

# mean weight in catch

png(paste0(output.dir,fn.prefix," - Catch weights.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- stock.data.pg@catch.wt

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) +
  geom_line(linewidth=0.4,aes(group=age,colour=factor(age))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize+ylim(0,NA)
print(p1)
dev.off()

# Catch weights - facet by age
png(paste0(output.dir,fn.prefix," - Catch weights by age.png"),width = 11, height = 7, units = "in", res = 600)

datFL <-stock.data.pg@catch.wt

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age)) + 
  facet_wrap(~age,scales="free_y")+geom_line(linewidth=0.4,aes(group=age,)) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",x="") +
 textSize+ylim(0,NA)
print(p1)
dev.off()

# mean weight in landings

png(paste0(output.dir,fn.prefix," - Landings weights.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- stock.data.pg@landings.wt
datFL[datFL ==0] <- NA

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) +
  geom_line(linewidth=0.4,aes(group=age,colour=factor(age))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize+ylim(0,NA)
print(p1)
dev.off()

# Landings weights - facet by age
png(paste0(output.dir,fn.prefix," - Landings weights by age.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- stock.data.pg@landings.wt
datFL[datFL ==0] <- NA

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age)) + 
  facet_wrap(~age,scales="free_y")+geom_line(linewidth=0.4,aes(group=age)) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",x="") +
textSize+ylim(0,NA)
print(p1)
dev.off()

# trends in mean weights for discard components

# by component
png(paste0(output.dir,fn.prefix," - Discard weights by age and component.png"),width = 11, height = 7, units = "in", res = 600)

tmp1 <- stock.data.pg@discards.wt
tmp1[tmp1==0] <- NA
tmp2 <- bmsw.pg
tmp2[tmp2==0] <- NA
tmp3 <- ibcw.pg
tmp3[tmp3==0] <- NA

datFL <- FLQuants(discards=tmp1,BMS=tmp2,IBC=tmp3)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=qname,colour=factor(qname))) + 
  facet_wrap(~age,scales="free_y")+geom_line(linewidth=0.4,aes(group=qname,colour=factor(qname))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize+ylim(0,NA)
print(p1)
dev.off()

# discards as SAM sees it
# combine discards, bms and ibc
dn <- readVPAFile(paste0(output.flr,"nor_had_cn_dis.txt")) # discard numbers
byn <- readVPAFile(paste0(output.flr,"nor_had_byn.txt")) # bycatch numbers
bn <- readVPAFile(paste0(output.flr,"nor_had_bmsn.txt")) # bms numbers
dw <- readVPAFile(paste0(output.flr,"nor_had_cw_dis.txt")) # discard weights
byw <- readVPAFile(paste0(output.flr,"nor_had_byw.txt")) # bycatch weights
bw <- readVPAFile(paste0(output.flr,"nor_had_bmsw.txt")) # bms weights

dn_new <- dn+byn+bn
dw_new <-  round((dn*dw+byn*byw+bn*bw)/(dn+byn+bn),3)
#dw_new[is.na(dw_new)] <- 0

# check 2006 and late 1980s mean weight issue
png(paste0(output.dir,fn.prefix," - Discards weights.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- trim(dw_new,age=0:8)
datFL["8",] <- quantSums(dn_new[ac(8:15),]*dw_new[ac(8:15),],na.rm=T)/quantSums(dn_new[ac(8:15),],na.rm=T)
datFL[datFL==0] <- NA

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age))) +
  geom_line(linewidth=0.4,aes(group=age,colour=factor(age))) +
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9) +textSize+ylim(0,NA)
print(p1)
dev.off()

# proportion discarded
dis.prop <- stock.data.pg@discards.n/stock.data.pg@catch.n
datFL <- FLQuants(dat = dis.prop)

png(paste0(output.dir,fn.prefix," - Proportion discarded at age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_line() + 
  facet_wrap(~age)+
  theme_bw()+ labs(y="Proportion discarded",x="") +textSize
print(p1)
dev.off()

# trends in mean weights by catch component
png(paste0(output.dir,fn.prefix," - Mean weight at age.png"),width = 12, height = 8, units = "in", res = 600)

datFL <- FLQuants(catch=stock.data.pg@catch.wt, landings=stock.data.pg@landings.wt, discards=stock.data.pg@discards.wt,
                  BMS=bmsw.pg,IBC=ibcw.pg)
datFL[["discards"]][datFL$discards ==0] <- NA
datFL[["BMS"]][bmsw.pg ==0] <- NA
datFL[["IBC"]][ibcw.pg ==0] <- NA
datFL[["landings"]]["0",] <- NA

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_point(aes(group=age,colour=factor(age)),size=0.7) + geom_smooth(span=1,se=F,aes(group=age,colour=factor(age)),linewidth=0.5)+
  facet_wrap(~qname,nrow=3)+
  theme_bw()+ labs(y="Mean weight-at-age (kg)",colour="",x="") +
  scale_colour_manual(values=col.pal9)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()


# catch proportion 
cat.prop <- stock.data.pg@discards.n %/% quantSums(stock.data.pg@catch.n)
datFL <- FLQuants(dat = cat.prop)

png(paste0(output.dir,fn.prefix," - Proportion caught at age.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=datFL, aes(year, data)) + xlim(c(1972,(ay-1)))+
  geom_line() + 
  facet_wrap(~age)+
  theme_bw()+ labs(y="Proportion caught",x="") +textSize
print(p1)
dev.off()

# percent of catch by year class (percentage bar with colour by age or cohort?)

# summarise age by year 
dat <- as.data.frame(stock.data.pg@catch.n)
age.dist <- dat %>% group_by(year,age) %>% summarise(num=sum(data,na.rm=T))
age.dist$age <- as.numeric(age.dist$age)
age.dist$year <- as.numeric(as.character(age.dist$year))

age.dist$cohort <- as.character(age.dist$year-age.dist$age)
age.dist$age <- as.factor(age.dist$age)

# attribute cohort colours
repn <- length(unique(age.dist$cohort))/length(col.pal)
col.pal <- rep(col.pal,repn)
cohortLst <- sort(unique(age.dist$cohort))
age.dist$col <- col.pal[match(age.dist$cohort,cohortLst)]

png(filename=paste0(output.dir,"Age distributions of catch numbers.png"), width=11, height=7, bg="white", pointsize=12, res=300, units="in")

p1 <- ggplot(age.dist,aes(x=year,y=num,fill=cohort))+# facet_wrap(~age, ncol=1,strip.position="right")+
  geom_col(position="fill")+theme_bw()+labs(x="Age",y="Number")+
  scale_fill_manual(values = age.dist$col)+
  # scale_colour_manual(values = age.dist$col)+
  theme_bw()+theme(legend.position = "none",axis.text.x = element_text(angle=45,hjust=1,vjust=1))
print(p1)
dev.off()

# commerical catch curves
windows(width = 10, height = 7)
par(mfrow = c(1,1), mar=c(3,4,3,3))
plot.catch.curve.and.grads(stock.data.pg, wk.ptype = "c", wk.ages = 2:4, 
                           wk.main = "", wk.yrs = 1980:(ay-1))
mtext("Northern Shelf haddock (had.27.46a20). Log commercial CPUE", side = 3, 
      line = -1.5, cex = 0.75, outer = TRUE)
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch curves.png"),type = "png")


# Commercial catch curve gradients
windows(width = 10, height = 7)
par(mfrow = c(1,1), mar = c(3,4,3,3))
plot.catch.curve.and.grads(stock.data.pg, wk.ptype = "g", wk.ages = 2:4, 
                           wk.main = "", wk.yrs = 1980:(ay-1))
mtext("Northern Shelf haddock (had.27.46a20). Commercial catch curve gradients", side = 3, line = -1, cex = 1.0, outer = TRUE)
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch curve gradients.png"),type = "png")

# Commercial catch correlations
windows(width = 10, height = 7)
plot.index.corr(list(FLIndex(catch.n = stock.data@catch.n, name = "Northern Shelf haddock (had.27.46a20). Commercial catch correlations")), 
                wk.type = "FLR",wk.name="Commerical catch")
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch correlations.png"),type = "png")


windows(width = 10, height = 7)
plot.index.corr(list(FLIndex(catch.n = stock.data.pg@catch.n, name = "Northern Shelf haddock (had.27.46a20). Commercial catch correlations")), 
                wk.type = "FLR",wk.name="Commerical catch")
savePlot(filename = paste0(output.dir,fn.prefix," - Commercial catch correlations pg.png"),type = "tiff")

graphics.off()

# Survey CPUE North Sea -------------------------------------------------
# Plot survey CPUE data #
cpue_dir <- "boot/data/DATRAS CPUE/"
load("boot/data/DATRAS CPUE/gmt3.RData")

### North Sea
# filename for North Sea  CPUE csv
csv_files <- dir(cpue_dir)[intersect(grep("NSQ",dir(cpue_dir)),grep(".csv",dir(cpue_dir)))]
file_nameQ1 <- csv_files[2]
file_nameQ3 <- csv_files[1]

# plot file name - basemap plots the last 5 years 
plot_filenameQ1<-paste0("Fig XXa HAD Q1_",ay-4,"-",ay,".png")
plot_filenameQ3<-paste0("Fig XXa HAD Q3_",ay-5,"-",ay-1,".png")

### IBTS Q1
cpue <- read.csv(paste0(cpue_dir,file_nameQ1), head=TRUE)
cpue <- cpue[cpue$Species=="Melanogrammus aeglefinus",]
names(cpue)[names(cpue)=="SubArea"] <- "Subarea"
cpue <- cpue[cpue$Area!=9,]
lonlat <- xy(cpue$Subarea)
gscale = 0.15

# Q1 Change species name for different species
png(paste0(output.dir,plot_filenameQ1), width=3000, height=2000) # don't forget to change fig name
par(mar=c(0,0.5,5,.5), lwd=2)
lay <- cbind(c(1,2,2),3:5,6:8,9:11,12:14,15:17)
layout(lay)

basemap(-4,13,51,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', line = 2, cex.main = 4)

plot(rep(.5,5),(1:5/5)-0.1, ylim=c(0,1), xlim=c(0,1), pch = 16, col = "lightblue",
     cex = gscale*sqrt(c(1000,2000,5000,10000,20000)), xlab = '', ylab = '', axes=FALSE)
points(rep(.5,5),(1:5/5)-0.1, cex = gscale*sqrt(c(1000,2000,5000,10000,20000)))
text(rep(.75,5),(1:5/5)-0.1, c(1000,2000,5000,10000,20000), cex=4.5)

# plot CPUE
for(y in (ay-4):ay){
  for(a in c('1','2','3+')){
    plotone(a,y,1)
    title(paste('Q1 ',y,': haddock age ',a,sep=''), 
          cex.main = 4, line = 2)
  }
}
dev.off()

### IBTS Q3
# read in data for Q3
cpue <- read.csv(paste0(cpue_dir,file_nameQ3), head=TRUE)
cpue<-cpue[cpue$Species=="Melanogrammus aeglefinus",]
names(cpue)[names(cpue)=="SubArea"] <- "Subarea"
cpue<-cpue[cpue$Area!=9,]
lonlat<-xy(cpue$Subarea)
gscale=0.15

png(paste0(output.dir,plot_filenameQ3), width=3000, height=2000) # don't forget to change fig name
par(mar=c(1,0.5,9,.5), lwd=2)
lay <- cbind(c(1,2,2,2),3:6,7:10,11:14,15:18,19:22)
layout(lay)

basemap(-4,13,51,62)
points(lonlat,pch='+', cex=2)
title('Survey Positions', line = 2, cex.main = 4)

plot(rep(.5,5),1:5/5, ylim=c(0,1), xlim=c(0,1), pch = 16, col = "lightblue",
     cex = gscale*sqrt(c(1000,2000,5000,10000,20000)), xlab = '', ylab = '', axes=FALSE)
points(rep(.5,5),1:5/5, cex = gscale*sqrt(c(1000,2000,5000,10000,20000)))
text(rep(.9,5),1:5/5, c(1000,2000,5000,10000,20000), cex=4)

# plot CPUE
for(y in (ay-5):(ay-1)){
  for(a in c('0','1','2','3+')){
    plotone(a,y,3)
    title(paste('Q3 ',y,': haddock age ',a,sep=''), 
          cex.main = 4, line = 2)
  }
}
dev.off()

# Survey indices ----------------------------------------------------------

load("data/indices.RData")

png(paste0(output.dir,fn.prefix," - Survey indices.png"),width = 11, height = 7, units = "in", res = 600)

datFL <- FLQuants(Q1=x.idx[[1]]@index, Q3Q4=x.idx[[2]]@index)

p1 <- ggplot(data=datFL, aes(year, data)) + 
  geom_line(aes(group=qname,colour=factor(qname)))+ theme_bw()+ labs(y="",colour="",x="") +
  facet_wrap(~age,scales="free")+
  scale_colour_manual(values=col.pal)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()

# splom plot

png(paste0(output.dir,fn.prefix," - Q1 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[1]], with.plus.pg = TRUE)
dev.off()

png(paste0(output.dir,fn.prefix," - Q3+Q4 survey splom plot.png"),width = 6, height = 6, pointsize=10, res=300, units="in")
survey.idx.splom(x.idx=x.idx[[2]], with.plus.pg = TRUE)
dev.off()



# Survey indices CVs --------------------------------------------------
cv1 <- read.table("data/Survey indices/survey-haddock-Q1-1-8plus_CV.dat",sep="\t",skip=5)
cv1[,1] <- 1983:ay
colnames(cv1) <- c("Year", 1:8)
cv1 <- pivot_longer(cv1,cols=ac(1:8),names_to="Age",values_to="CV")
cv1$label <- "Q1"

cv2 <- read.table("data/Survey indices/survey-haddock-Q3Q4-0-8plus_CV.dat",sep="\t",skip=5)
cv2[,1] <- 1991:(ay-1)
colnames(cv2) <- c("Year", 0:8)
cv2 <- pivot_longer(cv2,cols=ac(0:8),names_to="Age",values_to="CV")
cv2$label <- "Q3+Q4"

dat <- bind_rows(cv1,cv2)

png(paste0(output.dir,fn.prefix," - Survey indices CV.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=dat, aes(x=Year, y=CV,colour=label)) + 
  geom_line()+ theme_bw()+ labs(y="",x="",colour="") +
  facet_wrap(~Age)+  textSize
print(p1)
dev.off()



