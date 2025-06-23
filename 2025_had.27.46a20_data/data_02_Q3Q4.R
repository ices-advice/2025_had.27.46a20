
##  Combined index for Northern Shelf haddock - Q3Q4 
##  Data prep


# Set up
rm(list=ls())

# Settings
load("data/data_init.RData")
set.seed(1988)

# load data and QC -------------------------------------------------####

load(paste0("boot/data/dQ3Q4withALK_",ay-2,".RData"))

# check age - length 
xtabs(~LngtCm + Age, data=dQ3Q4[[1]])

hist(dQ3Q4[[1]]$LngtCm); range(dQ3Q4[[1]]$LngtCm)
hist(dQ3Q4[[1]]$Age); range(dQ3Q4[[1]]$Age)

# checks 
print("Records per survey, all years:")
print(table(dQ3Q4[[2]]$Year, dQ3Q4[[2]]$Survey))

summary(dQ3Q4[[1]])
summary(dQ3Q4[[2]])
summary(dQ3Q4[[3]])

summary(dQ3Q4[["HH"]]$HaulDur)
plot(dQ3Q4[["HH"]]$HaulDur)

summary(dQ3Q4[["HH"]]$lon)
summary(dQ3Q4[["HH"]]$lat)
plot(dQ3Q4[["HH"]]$lon,dQ3Q4[["HH"]]$lat)

summary(dQ3Q4[["HL"]]$LngtCm)
hist(dQ3Q4[["HL"]]$LngtCm)

summary(dQ3Q4[["HL"]]$Count)
plot(dQ3Q4[["HL"]]$LngtCm,dQ3Q4[["HL"]]$Count)

summary(dQ3Q4[["CA"]]$Age)
hist(dQ3Q4[["CA"]]$Age)

# Make plots of Age vs. LngtCm.
plot(jitter(dQ3Q4[["CA"]]$Age),dQ3Q4[["CA"]]$LngtCm)

# Add new year data ----------------------------------------------####

##### NS-IBTS

NS.IBTS <- getDatrasExchange("NS-IBTS", years=ay-1, quarters=3, strict=FALSE)
##### Select only haddock data and valid hauls
NS.IBTS <- subset(NS.IBTS, Species==paste(genus, bfamily) , HaulVal=="V", StdSpecRecCode==1)

##### Assign the groundgear type
NS.IBTS[[2]]$Gear <- as.character(NS.IBTS[[2]]$Gear)
NS.IBTS[[2]]$Year <- as.numeric(as.character(NS.IBTS[[2]]$Year)) 
NS.IBTS[[2]][NS.IBTS[[2]]$Gear=="GOV", "Gear"] <- "GOVA"
NS.IBTS[[2]][substr(NS.IBTS[[2]]$Gear, 1, 3)=="GOV" & NS.IBTS[[2]]$Year >= 1985 & NS.IBTS[[2]]$ShootLat >= 57.5 & NS.IBTS[[2]]$Country=="GB-SCT", "Gear"] <- "GOVB"
NS.IBTS[[2]]$Gear <- factor(NS.IBTS[[2]]$Gear)
NS.IBTS[[2]]$Year <- factor(NS.IBTS[[2]]$Year)
NS.IBTS[[3]]$Gear <- NS.IBTS[[2]][match(NS.IBTS[[3]]$haul.id, NS.IBTS[[2]]$haul.id), "Gear"]
NS.IBTS[[1]]$Gear <- NS.IBTS[[2]][match(NS.IBTS[[1]]$haul.id, NS.IBTS[[2]]$haul.id), "Gear"]

##### IE-IGFS

IE.IGFS <- getDatrasExchange("IE-IGFS", years=ay-1, quarters=4, strict=FALSE)

##### Select only haddock data and valid hauls
IE.IGFS <- subset(IE.IGFS, Species==paste(genus, bfamily), HaulVal=="V", StdSpecRecCode==1)

##### Assign the groundgear type
IE.IGFS[[1]]$Gear <- factor("GOVD"); IE.IGFS[[2]]$Gear <- factor("GOVD"); IE.IGFS[[3]]$Gear <- factor("GOVD")

##### SCOWCGFS

SCOWCGFS <- getDatrasExchange("SCOWCGFS", years=ay-1, quarters=4, strict=FALSE)

##### Select only haddock data and valid hauls
SCOWCGFS <- subset(SCOWCGFS, Species==paste(genus, bfamily), HaulVal=="V", StdSpecRecCode==1)

##### Assign the groundgear type
SCOWCGFS[[1]]$Gear <- factor("GOVD"); SCOWCGFS[[2]]$Gear <- factor("GOVD"); SCOWCGFS[[3]]$Gear <- factor("GOVD")



## combine
dAll <- c(NS.IBTS, IE.IGFS, SCOWCGFS)

dAll <- addSpatialData(dAll, "boot/data/ICES_areas/ICES_areas.shp")

##### Round lengths to 1 cm
dAll[[1]]$LngtCm <- round(dAll[[1]]$LngtCm); dAll[[3]]$LngtCm <- round(dAll[[3]]$LngtCm)


# Data checks 
print("Records per survey, all years:")
print(table(dAll[[2]]$Year, dAll[[2]]$Survey))

summary(dAll[[1]])
summary(dAll[[2]])
summary(dAll[[3]])

summary(dAll[["HH"]]$HaulDur)
plot(dAll[["HH"]]$HaulDur)

summary(dAll[["HH"]]$lon)
summary(dAll[["HH"]]$lat)
plot(dAll[["HH"]]$lon,dAll[["HH"]]$lat)

summary(dAll[["HL"]]$LngtCm)
hist(dAll[["HL"]]$LngtCm)

summary(dAll[["HL"]]$Count)
plot(dAll[["HL"]]$LngtCm,dAll[["HL"]]$Count)

summary(dAll[["CA"]]$Age)
hist(dAll[["CA"]]$Age)

# Make plots of Age vs. LngtCm.
plot(jitter(dAll[["CA"]]$Age),dAll[["CA"]]$LngtCm)

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

# Process survey data -----------------------------------------------------------------------------------##

# join
dQ3Q4 <- c(dQ3Q4, dAll)

# add spectrum
dQ3Q4 <- addSpectrum(dQ3Q4, by=cmSize)

##### Impute missing depths
summary(dQ3Q4$Depth)
if(length(which(is.na(dQ3Q4$Depth)))>0){
  
  dmodel <- gam(log(Depth) ~ s(lon, lat, k=200), data=dQ3Q4[[2]])
  sel <- subset(dQ3Q4, is.na(Depth))
  sel$Depth <- 0; ##### Guard against NA-error
  dQ3Q4$Depth[is.na(dQ3Q4$Depth)] = exp(predict(dmodel, newdata=sel[[2]]))
  dmodel <- NULL
  sel <- NULL
  gc()
}

# Select survey time (Jun-Dec) and areas
dQ3Q4 <- subset(dQ3Q4, Month >= 6, Month <= 12)

table(dQ3Q4[[2]]$ICESAREA)
sel.areas <- c("IIIa20", "IVa", "IVb", "IVc", "VIa")
dQ3Q4 <- subset(dQ3Q4, ICESAREA %in% as.character(sel.areas))


# Catch weight by haul
dQ3Q4 <- addWeightByHaul(dQ3Q4)

# Gear subsetting
print(xtabs(~Year + Gear, data=dQ3Q4[[2]]))
print(xtabs(~Year + Survey, data=dQ3Q4[[2]]))

# Age-length key ---------------------------------------------------------------------####

##### Check for enough age data
xtabs(NoAtALK~Year+Survey, data = dQ3Q4[[1]])
xtabs(NoAtALK~Year+Age, data = dQ3Q4[[1]])
xtabs(~LngtCm + Age, data=dQ3Q4[[1]])

noagesamples <- xtabs(!is.na(Age)~Year, data=dQ3Q4[[1]])

removeAgeNAs <- function(x) {
  x[[1]] <- subset(x[[1]], !is.na(x[[1]]$Age))
  x[[1]] <- subset(x[[1]], !is.na(x[[1]]$NoAtALK))
  x
}

dQ3Q4 <- removeAgeNAs(dQ3Q4)

sink("output/indices/Q3Q4noages.txt")
xtabs(NoAtALK~Year+Age, data=dQ3Q4[[1]])
sink()

## Declare settings for ALK model
mf <- "" 
ack <- TRUE
useBICs <- TRUE
varCofs <- FALSE
maxKs <- 50
mc.cores <- 1 ## Windows users should use mc.cores=1

add.ALK <- function(d){
  
  ages<- agesQ3Q4
  
  if(d$Quarter[1]=="1"){
    ages <- NA
  }
  d[[1]] <- subset(d[[1]], Age >= min(ages))
  for(aa in ages){
    d <- fixAgeGroup(d, age=aa, n=1, fun=ifelse(aa==min(ages), min, mean))
  }
  
  d <- addSpectrum(d, by=cmSize)
  
  d.ysplit <- split(d, d$Year)
  
  d.ALK <- mclapply(d.ysplit, fitALK, minAge=min(ages), maxAge=max(ages), autoChooseK=ack, useBIC=useBICs, varCof=varCofs, maxK=maxKs, mc.cores=mc.cores, method=3)
  
  d.Nage <- mclapply(d.ALK, predict, mc.cores=mc.cores)
  for(i in 1:length(d.ALK)) d.ysplit[[i]]$Nage=d.Nage[[i]];
  dd <- do.call("c", d.ysplit)
  dd    
}

##### Please wait...
dQ3Q4 <- add.ALK(dQ3Q4)

save(dQ3Q4, file=paste0("data/dQ3Q4withALK_",ay-1,".RData"))

