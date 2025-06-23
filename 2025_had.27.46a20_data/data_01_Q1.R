
##  Combined index for Northern Shelf haddock - Q1 
##  Data prep


# Set up
rm(list=ls())

# Settings
load("data/data_init.RData")
set.seed(1988)

# load data and QC -------------------------------------------------####

load(paste0("boot/data/dQ1withALK_",ay-1,".RData"))

# remove fish - age 1 and 42 cm!! Error
dQ1[[1]] <- dQ1[[1]][!(dQ1[[1]]$Year==2001 & dQ1[[1]]$Age==1 & dQ1[[1]]$LngtClas==42), ]

# Round lengths to 1 cm
dQ1[[1]]$LngtCm <- round(dQ1[[1]]$LngtCm); dQ1[[3]]$LngtCm <- round(dQ1[[3]]$LngtCm)

# Add new year data ----------------------------------------------####

#### NS-IBTS
if(!file.exists(paste0("data/NS exchange data haddock Q1 ",ay,".RData"))){
NS.IBTS <- getDatrasExchange("NS-IBTS", years=ay, quarters=1, strict=FALSE)
# Select only haddock data and valid hauls
NS.IBTS <- subset(NS.IBTS, Species==paste(genus, bfamily) , HaulVal=="V", StdSpecRecCode==1)

# save for use in maturity analysis
save(NS.IBTS,file=paste0("data/NS exchange data haddock Q1 ",ay,".RData"))
}else{
  load(paste0("data/NS exchange data haddock Q1 ",ay,".RData"))
}

# Assign the groundgear type
NS.IBTS[[2]]$Gear <- as.character(NS.IBTS[[2]]$Gear)
NS.IBTS[[2]]$Year <- as.numeric(as.character(NS.IBTS[[2]]$Year)) 
NS.IBTS[[2]][NS.IBTS[[2]]$Gear=="GOV", "Gear"] <- "GOVA"
NS.IBTS[[2]][substr(NS.IBTS[[2]]$Gear, 1, 3)=="GOV" & NS.IBTS[[2]]$Year >= 1985 & NS.IBTS[[2]]$ShootLat >= 57.5 & NS.IBTS[[2]]$Country=="GB-SCT", "Gear"] <- "GOVB"
NS.IBTS[[2]]$Gear <- factor(NS.IBTS[[2]]$Gear)
NS.IBTS[[2]]$Year <- factor(NS.IBTS[[2]]$Year)
NS.IBTS[[3]]$Gear <- NS.IBTS[[2]][match(NS.IBTS[[3]]$haul.id, NS.IBTS[[2]]$haul.id), "Gear"]
NS.IBTS[[1]]$Gear <- NS.IBTS[[2]][match(NS.IBTS[[1]]$haul.id, NS.IBTS[[2]]$haul.id), "Gear"]

#### SCOWCGFS

if(!file.exists(paste0("data/WC exchange data haddock Q1 ",ay,".RData"))){
SCOWCGFS <- getDatrasExchange("SCOWCGFS", years=ay, quarters=1, strict=FALSE)
# Select only haddock data and valid hauls
SCOWCGFS <- subset(SCOWCGFS, Species==paste(genus, bfamily) , HaulVal=="V", StdSpecRecCode==1)

save(SCOWCGFS,file=paste0("data/WC exchange data haddock Q1 ",ay,".RData"))
}else{
  load(paste0("data/WC exchange data haddock Q1 ",ay,".RData"))
}
# Assign the groundgear type
SCOWCGFS[[1]]$Gear <- factor("GOVD"); SCOWCGFS[[2]]$Gear <- factor("GOVD"); SCOWCGFS[[3]]$Gear <- factor("GOVD")


## combine
dAll <- c(NS.IBTS, SCOWCGFS)

dAll <- addSpatialData(dAll, "boot/data/ICES_areas/ICES_areas.shp")

##### Round lengths to 1 cm
dAll[[1]]$LngtCm <- round(dAll[[1]]$LngtCm); dAll[[3]]$LngtCm <- round(dAll[[3]]$LngtCm)

# Data checks ----------------------------------------------------------------
print("Records per survey, all years:")
print(table(dAll[[2]]$Year, dAll[[2]]$Survey))

summary(dAll[[1]])
summary(dAll[[2]])
summary(dAll[[3]])

dat <- c(dQ1,dAll)
dat <- subset(dat,as.character(Year) %in% c("2023","2024","2025"))

# check HH


# check explanatory variables
dat1 <- dat[[2]]
dat2 <- as.data.frame(dat$Nage)
dat2$haul.id <- dat1$haul.id
dat2$Year <- dat1$Year
dat2$Gear <- dat1$Gear
dat2$Ship <- dat1$Ship
dat2$lon <- dat1$lon
dat2$lat <- dat1$lat
dat2$Depth <- dat1$Depth
dat2$TimeShotHour <- dat1$TimeShotHour
dat2$timeOfYear <- dat1$timeOfYear
dat2$HaulDur <- dat1$HaulDur

# pivot
dat2 <- pivot_longer(dat2,cols=c(1:7,"8+"),names_to="Age",values_to="N")

windows()
ggplot(dat2,aes(x=Year,y=log(N)))+facet_wrap(~Age)+geom_boxplot()
ggplot(dat2,aes(x=interaction(Year,Gear),y=log(N),colour=Year))+facet_wrap(~Age)+geom_boxplot()
ggplot(dat2,aes(x=interaction(Year,Ship),y=log(N),colour=Year))+facet_wrap(~Age)+geom_boxplot()
ggplot(dat2,aes(x=lon,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()
ggplot(dat2,aes(x=lat,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()
ggplot(dat2,aes(x=Depth,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()
ggplot(dat2,aes(x=TimeShotHour,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()
ggplot(dat2,aes(x=timeOfYear,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()
ggplot(dat2,aes(x=HaulDur,y=log(N),colour=Year))+facet_wrap(~Age)+geom_point()

# check NAs
summary(dat2)

coln <- colnames(dat2)
lapply(coln,function(x){sum(is.na(dat2[,x]))})

# check ranges
mindat <- dat2  %>% group_by(Year) %>%
  mutate(across(c(lon,lat,Depth,TimeShotHour,timeOfYear,HaulDur), min))

maxdat <- dat2  %>% group_by(Year) %>%
  mutate(across(c(lon,lat,Depth,TimeShotHour,timeOfYear,HaulDur), max))

tt <- bind_rows(mindat,maxdat)
for (x in c("lon","lat","Depth","TimeShotHour","timeOfYear","HaulDur")){
  windows()
ggplot(tt,aes(x=Year,y=x))+geom_point()
}



# check CA 
ca <- dat[[1]]
# NAs
coln <- colnames(ca)
ll <- lapply(coln,function(x){sum(is.na(ca[,x]))})
idx <- which(!ll==0)

summary(ca[,idx]) # these columns are ok to have NAs

# age length plots

ggplot(ca,aes(x=(as.character((Age))),y=LngtCm))+facet_wrap(~Year)+geom_boxplot()
ggplot(ca,aes(x=Year,y=LngtCm))+facet_wrap(~Age)+geom_boxplot()

#write.csv(dQ1[[1]],file="check ca.csv",row.names=F)

# check HL

hl <- dat[[3]]

# NAs
coln <- colnames(hl)
ll <- lapply(coln,function(x){sum(is.na(hl[,x]))})
idx <- which(!ll==0)

summary(hl[,idx]) # these columns are ok to have NAs

# check N at length
N <- xtabs(Count ~ haul.id + sizeGroup, data =hl)
N <- as.data.frame(N)
N$Year <- substring(N$haul.id,first = 1,last=4)

windows()
ggplot(N,aes(x=sizeGroup,y=log(Freq)))+facet_wrap(~Year)+geom_boxplot()
ggplot(hl,aes(x=as.factor(LngtCm),y=log(Count)))+facet_wrap(~Year)+geom_boxplot()
ggplot(hl,aes(x=as.factor(LngtCm),y=log(HLNoAtLngt)))+facet_wrap(~Year)+geom_boxplot()

# check hauls
 hl.yr <- filter(hl.yr,as.character(Year) == 2025)
 ggplot(hl.yr,aes(x=haul.id,y=log(Count)))+facet_wrap(~Ship,sclaes="free")+geom_boxplot()

# Where possible, make a map with ShootLong/ShootLat- HaulLong/HaulLat. Compare the haul length with the nearby hauls.
windows()
tmp <- dAll[["HH"]]

# haul ID col scale
durs <- sort(unique(tmp$HaulDur))
col <- rainbow(n=length(durs))
tmp$col <- col[match(tmp$HaulDur,durs)]

plot(tmp$ShootLong-tmp$HaulLong,tmp$ShootLat-tmp$HaulLat)

plot(tmp$ShootLong,tmp$ShootLat,col=tmp$col)
legend("bottomright",inset=0.02,col=col,legend=durs,pch=1)

plot(tmp$HaulLong,tmp$HaulLat,col=tmp$col)
legend("bottomright",inset=0.02,col=col,legend=durs,pch=1)

#Check Depth. If missing, try to estimate it from lon/lat. 
summary(dAll[["HH"]]$Depth)
hist(dAll[["HH"]]$Depth)

dpts <- sort(unique(tmp$Depth))
col <- rainbow(n=length(dpts))
tmp$col <- col[match(tmp$Depth,dpts)]

plot(tmp$HaulLong,tmp$HaulLat,col=col)
legend("bottomright",inset=0.02,col=col,legend=dpts,pch=1)


# corrections --------------------------------------------------------------------

# Age 2 fish at 43 cm? Uncertain- remove
#dAll[[1]][(dAll[[1]]$Year==2025 & dAll[[1]]$Age==2 & dAll[[1]]$LngtCm>40), ]

#dAll[[1]] <- dAll[[1]][!(dAll[[1]]$Year==2025 & dAll[[1]]$Age==2 & dAll[[1]]$LngtClas>40), ]

# Very high HL count value for this haul - looks odd
tmp <- dAll[[3]]
idx <- which(tmp$haul.id %in% "2025:1:SE:77SE:GOV:7:6" & tmp$LngtCm == 28)
tmp[idx,]
dAll[[3]] <- dAll[[3]][-idx,]

# Process survey data -----------------------------------------------------------------------------------##

dQ1.backup <- dQ1
# dQ1 <- dQ1.backup

#join
dQ1 <- c(dQ1, dAll)

# Add spectrum
dQ1 <- addSpectrum(dQ1, by=cmSize)

##### Impute missing depths
summary(dQ1$Depth)
if(length(which(is.na(dQ1$Depth)))>0){
  ##### Error in depth !!!!! (just one record). Changed to NA. 
  # dQ1[[2]][!is.na(dQ1$Depth) & dQ1$Depth==0, ]
  dQ1[[2]][!is.na(dQ1$Depth) & dQ1$Depth==0, "Depth"] <- NA
  
  dmodel <- gam(log(Depth) ~ s(lon, lat, k=200), data=dQ1[[2]])
  sel <- subset(dQ1, is.na(Depth))
  sel$Depth <- 0 ##### Guard against NA-error
  dQ1$Depth[is.na(dQ1$Depth)] <- exp(predict(dmodel, newdata=sel[[2]]))
  rm(dmodel, sel)
  gc()
}

# Select survey time (up to early April) and areas
dQ1 <- subset(dQ1, Month >= 1, Month <= 4)

table(dQ1[[2]]$ICESAREA)
sel.areas <- c("IIIa20", "IVa", "IVb", "IVc", "VIa")
dQ1 <- subset(dQ1, ICESAREA %in% as.character(sel.areas))

# Catch weight by haul
dQ1 <- addWeightByHaul(dQ1)
hist(dQ1[["HH"]]$HaulWgt)
plot(dQ1[["HH"]]$Year,dQ1[["HH"]]$HaulWgt)

# Gear subsetting
print(xtabs(~Year + Gear, data=dQ1[[2]]))
print(xtabs(~Year + Survey, data=dQ1[[2]]))


# Age-length key ---------------------------------------------------------------------####

##### Check for enough age data
xtabs(NoAtALK~Year+Survey, data = dQ1[[1]])
xtabs(NoAtALK~Year+Age, data = dQ1[[1]])
xtabs(~LngtCm + Age, data=dQ1[[1]])

noagesamples <- xtabs(!is.na(Age)~Year, data=dQ1[[1]])

removeAgeNAs <- function(x) {
  x[[1]] <- subset(x[[1]], !is.na(x[[1]]$Age))
  x[[1]] <- subset(x[[1]], !is.na(x[[1]]$NoAtALK))
  x
}

dQ1 <- removeAgeNAs(dQ1)

sink("output/indices/Q1noages.txt")
xtabs(NoAtALK~Year+Age, data=dQ1[[1]])
sink()

## Declare settings for ALK model
mf <- "" 
ack <- TRUE
useBICs <- TRUE
varCofs <- FALSE
maxKs <- 50
mc.cores <- 1 ## Windows users should use mc.cores=1

d <- dQ1
add.ALK <- function(d){
  
  ages <- agesQ1[2]
  
  if (d$Quarter[1]=="1") {
    ages <- agesQ1
  }
  
  d[[1]] <- subset(d[[1]], Age >= min(ages))
  for(aa in ages){
    d <- fixAgeGroup(d, age=aa, n=1, fun=ifelse(aa==min(ages), min, mean))
  }
  
  d <- addSpectrum(d, by=cmSize)
  
  d.ysplit <- split(d, d$Year)
 
  d.ALK <- mclapply(d.ysplit, fitALK, minAge=min(ages), maxAge=max(ages), autoChooseK=ack, useBIC=useBICs, varCof=varCofs, maxK=maxKs, mc.cores=mc.cores, method=3)
  
  d.Nage <- mclapply(d.ALK, predict, mc.cores=mc.cores)
  for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]]
  dd <- do.call("c", d.ysplit)
  dd    
}

##### Please wait...
dQ1 <- add.ALK(dQ1)

save(dQ1, file=paste0("data/dQ1withALK_",ay,".RData"))

