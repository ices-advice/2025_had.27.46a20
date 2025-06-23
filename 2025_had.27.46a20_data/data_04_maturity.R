#######################################################################
# Maturity ogive calculations for Northern Shelf haddock              
# COMBINED SEXES                                                      
# Northern Shelf region                                               
# Author: Aaron Brazier                                               
# Date created: 24/05/2021 12:15:18                                   
# Last edited: 21/03/2025                                   
# Version: 3                                                          
#######################################################################

start.tt <- now()

#Set workspace parameters
load("data/data_init.RData")
fyear <- ay
years   <- 1990:fyear
ages    <- agesQ1
sexes   <- c("F","M")


# Step 1. load pre-processed survey data -----------------------------------

load("data/pre-processed survey data.RData")


# Step 2. Data manipulation ---------------------------------------------------

# remove missing maturity values (NA's)
haddock <- subset(haddock, !is.na(Maturity))

# check data
dat <- haddock[["CA"]]

windows()
dat1 <- dat %>% group_by(Age,LngtCm,Country) %>% summarise(num=sum(NoAtALK))

p1 <- ggplot(dat1,aes(x=Age,y=LngtCm))+geom_point()+facet_wrap(~Country)
print(p1)

dat1 <- dat %>% filter(!is.na(IndWgt)) %>% group_by(Age,LngtCm,Country) %>% summarise(num=sum(NoAtALK),numwt=sum(NoAtALK*IndWgt,na.rm=T))
dat1$wt <- dat1$numwt/dat1$num

p1 <- ggplot(dat1,aes(x=Age,y=wt))+geom_point()+facet_wrap(~Country)
print(p1)
dev.off()

#Reassign binary response variable to ICES maturity codes in hl table
#(1 = mature, 0 = immature)

tmp <- haddock[["CA"]]
tmp <- aggregate(tmp$NoAtALK,by=list(Year=tmp$Year,Country=tmp$Country,Maturity=tmp$Maturity),sum,na.rm=T)
write.csv(tmp,file="data/Maturity/Samples per country per maturity code.csv",row.names=F)

print(sort(unique(haddock[["CA"]]$Maturity)))
# filter out codes: 6, 66, F (all mean "abnormal")
haddock[["CA"]] <- subset(haddock[["CA"]], 
                          Maturity %in% c("1","2","3","4","5","61","62","63","64","65","I","M","A",
                                          "B","Ba","Bb","C","Ca","Cb","D","Da","Db","E"))
# recode codes 1, 61, A, Ba and I as immature (0)
haddock[["CA"]]$Maturity <- ifelse(haddock[["CA"]]$Maturity == "1", "0",
                                   ifelse(haddock[["CA"]]$Maturity == "61", "0",
                                          ifelse(haddock[["CA"]]$Maturity == "A", "0",
                                                 ifelse(haddock[["CA"]]$Maturity == "Ba", "0",
                                                        ifelse(haddock[["CA"]]$Maturity == "I", "0", "1")))))
print(sort(unique(haddock[["CA"]]$Maturity)))

haddock[["CA"]]$Maturity <- as.numeric(as.character(haddock[["CA"]]$Maturity))

#Subset sexes and ages
haddock <- subset(haddock, Year %in% years)
haddock <- subset(haddock, Sex %in% sexes)
haddock <- subset(haddock, Age %in% ages) 

# Step 3. Create SMALK from CA data --------------------------------------

#Total numbers per haul
haddock[["HH"]]$nbL <- rowSums(haddock[["HH"]]$N)
smalk <- haddock[["CA"]]

#Simplify SMALK dataset and maturity rating
smalk$survey <- smalk$Survey
smalk$year   <- smalk$Year
smalk$length <- smalk$LngtCm
smalk$age    <- smalk$Age
smalk$strec  <- smalk$AreaCode
smalk$sex    <- smalk$Sex
smalk$mat    <- smalk$Maturity
smalk$nb     <- smalk$NoAtALK

smalk  <- smalk[,c("year","haul.id","length","age","strec","sex","mat","nb")]

#Remove unnecassary variables from SMALK dataframe
smalk$area <- rep(0,dim(smalk)[1])
for(i in 1:dim(smalk)[1]){
  if(smalk$strec[i] %in% east)smalk$area[i]   <- "east"
  if(smalk$strec[i] %in% west)smalk$area[i]   <- "west"
  if(smalk$strec[i] %in% Chann)smalk$area[i]  <- "Chann"
  if(smalk$strec[i] %in% Celtic)smalk$area[i] <- "Celtic"
  if(smalk$strec[i] %in% NorSka)smalk$area[i] <- "NorSka"
}

smalk <- droplevels(subset(smalk, !strec == 0))
smalk <- droplevels(subset(smalk, !area == 0))
smalk <- droplevels(subset(smalk, age >= 0))

smalk$area <- as.factor(smalk$area)

#Remove areas in the Channel, the Celtic Sea, and Norwegian coast of Skagerrak
smalk <- droplevels(subset(smalk, !area=="Chann"))
smalk <- droplevels(subset(smalk, !area=="Celtic"))
smalk <- droplevels(subset(smalk, !area=="NorSka"))

smalk$length <- floor(smalk$length)

print("SMALK done")
print(now()-start.tt)


# Step 4. Get a catch rate: East against West -------------------------------------------

# First calc the catch rate for plotting
catr <- haddock[["HH"]]

catr <- catr[,c("haul.id","Year","StatRec","nbL")]    

catch <- catr %>% group_by(Year,StatRec) %>% summarise(nbL=sum(nbL))
colnames(catch) <- c("year","area","nbL")

#Remove unnecessary variables 
catch$area2 <- rep(0,dim(catch)[1])
for(i in 1:dim(catch)[1]){
  if(catch$area[i] %in% east)catch$area2[i]   <- "east"
  if(catch$area[i] %in% west)catch$area2[i]   <- "west"
  if(catch$area[i] %in% Chann)catch$area2[i]  <- "Chann"
  if(catch$area[i] %in% Celtic)catch$area2[i] <- "Celtic"
  if(catch$area[i] %in% NorSka)catch$area2[i] <- "NorSka"
}

catch <- droplevels(subset(catch,!area2 == 0))
catch <- droplevels(subset(catch,!area2 == "Chann"))
catch <- droplevels(subset(catch,!area2 == "Celtic"))
catch <- droplevels(subset(catch,!area2 == "NorSka")) 

catch$year <- as.numeric(as.character(catch$year))
catch <- catch[order(catch$year,catch$area,catch$area2,catch$nbL),]

#Get catch rate per area (area2)
catr <- aggregate(catch$nbL, list(catch$year,catch$area2), sum)
colnames(catr) <- c("year","area","nbL")

#Get an east and west data frame
east_cr <- subset(catr, catr$area=="east")
west_cr <- subset(catr, catr$area=="west")
colnames(east_cr)[3] <- "nbL.e"
colnames(west_cr)[3] <- "nbL.w"

catr <- cbind(east_cr,west_cr)
catr <- catr[,c(1,3,6)]

catr$nbL.ns <- catr$nbL.e + catr$nbL.w

catr$nbL.e <- catr$nbL.e / 1000
catr$nbL.w <- catr$nbL.w / 1000
catr$nbL.ns <- catr$nbL.ns / 1000

write.taf(catr, "data/Maturity/Catch_Rate_By_Area.csv")

# Second, calc weighting for data based on catch rate
subHH <- droplevels(haddock[["HH"]])

subHH$area2 <- rep(0,dim(subHH)[1])
for(i in 1:dim(subHH)[1]){
  if(subHH$StatRec[i] %in% east)subHH$area2[i]   <- "east"
  if(subHH$StatRec[i] %in% west)subHH$area2[i]   <- "west"
  if(subHH$StatRec[i] %in% Chann)subHH$area2[i]  <- "Chann"
  if(subHH$StatRec[i] %in% Celtic)subHH$area2[i] <- "Celtic"
  if(subHH$StatRec[i] %in% NorSka)subHH$area2[i] <- "NorSka"
}

subHH$area2 <- as.factor(subHH$area2)

#Remove the Channel, areas in the Celtic Sea and Norwegian coast of Skagerrak
subHH <- droplevels(subset(subHH,!area2 == "Chann"))
subHH <- droplevels(subset(subHH,!area2 == "Celtic"))
subHH <- droplevels(subset(subHH,!area2 == "NorSka"))
subHH <- droplevels(subset(subHH,!area2 == "0"))

#Mean values per rectangle first; new
rez1 <- subHH %>% group_by(Year,StatRec) %>% summarize(nbL=mean(nbL),HaulWgt=mean(HaulWgt),area2=unique(area2))

#Mean values per area for all rectangles
rez <- rez1 %>% group_by(Year,area2) %>% summarize(nbL=mean(nbL),HaulWgt=mean(HaulWgt)) 

erec <- length(unique(subHH[subHH$area2=="east","StatRec"]))
wrec <- length(unique(subHH[subHH$area2=="west","StatRec"]))

rez$proparea <- NA
rez[rez$area2 == "east","proparea"] <- erec/sum(erec+wrec)
rez[rez$area2 == "west","proparea"] <- wrec/sum(erec+wrec)

rez$nbL_2     <- rez$proparea * rez$nbL  
rez$HaulWgt_2 <- rez$proparea * rez$HaulWgt

rezT <- rez %>% group_by(Year) %>% summarize(Tnb=sum(nbL_2), TWgt=sum(HaulWgt_2)) # total

rezF <- left_join(rez,rezT)

rezF$wt <- rezF$nbL_2/rezF$Tnb

rezF$Year <- as.numeric(as.character(rez$Year))

rezE <- subset(rezF,area2=="east")
rezW <- subset(rezF,area2=="west")

write.taf(rezF, "data/Maturity/rezF_mat.csv")

rezF <- rezF[,c(1:2,6)]

rezFT <- rezF %>% group_by(Year) %>% summarise(tot=sum(nbL_2))
rezF <- left_join(rezF,rezFT)

rezF$wtC <- rezF$nbL_2 / rezF$tot

rezF <- rezF[,c(1,2,5)]


print("Catch rate weightings done")
print(now()-start.tt)


# Step 6. Generate final dataset ----------------------------------------------------

# individual sexes, no sex ratio
dataG_indvsex_nsr <- smalk %>% group_by(haul.id,age,sex,length,mat) %>% 
  summarise(nb=sum(nb), year=unique(year), area=unique(area))

dataG <- dataG_indvsex_nsr


#drop maturity status for getting weights
dataG1_indvsex_nsr <- smalk %>% group_by(haul.id,age,length) %>% 
  summarise(nb=sum(nb), year=unique(year), area=unique(area))

dataG1 <- dataG1_indvsex_nsr

dataG_filename <- "dataG_individual_sexes_no_sexratio.csv"


print("dataG final dataset created")
print(now()-start.tt)

#------------------------------------------------------------------------------#
# Step 7. Generate weighting factors ----------####
#Generate weighting factor using CEFAS equation: wi = ma x rg/Ra


#Generate intermediate data for length
wL <- as.data.frame(haddock[["HH"]]$N)
wL$haul.id <- haddock[["HH"]]$haul.id
rownames(wL) <- c()


#Produce the rg raising factor for each fish
#dataG1$rg <- rep(-1, dim(dataG1)[1])

wL1 <- droplevels(subset(wL, haul.id %in% dataG$haul.id))
wL1$length <- as.numeric(sub('.([^,]+),.*','\\1', wL1$sizeGroup))

i <- which(dataG1$haul.id %in% "2006:1:DE:06NI:GOV:92:14" & dataG1$age ==4)[1]

# for(i in 1:dim(dataG1)[1]){
#   ww<-subset(subset(wL1,haul.id==dataG1$haul.id[i]),length==dataG1$length[i]) 
#   
#   nn<-sum(subset(subset(dataG1,haul.id==dataG1$haul.id[i]),length==dataG1$length[i])$nb) # new 2022, same weight irrespective of maturity status
#   
#   dataG1$rg[i]<-max(nn,ww$Freq)/nn  #new 2022
#   
# }

dataG1 <- left_join(dataG1,wL1,by=c("haul.id","length")) 
tmp <- dataG1 %>% group_by(haul.id,length) %>% summarise(nn=sum(nb))
dataG1 <- left_join(dataG1,tmp,by=c("haul.id","length")) 
dataG1 <- dataG1 %>% rowwise() %>% mutate(rg = max(nn,Freq)/nn)
dataG1 <- dataG1 %>% select(!c(sizeGroup,Freq))

dataG1[i,]
#Produce the Ra raising factor
#dataG1$Ra <- rep(-1, dim(dataG1)[1])

# for(i in 1:dim(dataG1)[1]){
#   dataG1$Ra[i] <- sum(subset(subset(dataG1, haul.id == dataG1$haul.id[i]), age == dataG1$age[i])$rg)
# }

tmp <- dataG1 %>% group_by(haul.id,age) %>% summarise(Ra=sum(rg))
dataG1 <- left_join(dataG1,tmp)

#Produce the wi raising factor
#dataG1$wi <- rep(-1, dim(dataG1)[1])

# for(i in 1:dim(dataG1)[1]){
#   dataG1$wi[i] <- sum(subset(subset(dataG1, haul.id == dataG1$haul.id[i]),
#                              age == dataG1$age[i])$nb) * dataG1$rg[i] / dataG1$Ra[i]
# }

tmp <- dataG1 %>% group_by(haul.id,age) %>% summarise(nbT=sum(nb))
dataG1 <- left_join(dataG1,tmp,by=c("haul.id","age"))
dataG1 <- dataG1 %>% rowwise() %>% mutate(wi=nbT*rg/Ra)
dataG1 <- dataG1 %>% select(!c(nbT))

#merge dataG and dataG1 , same weight for different maturity states, the assign proportion so they sum
#dataGfin<- merge(dataG, dataG1, by=c( "year","area","haul.id", "age", "length"), all=TRUE)  # new 2022
dataGfin <- left_join(dataG,dataG1,by=c( "year","area","haul.id", "age", "length"))
dataGfin$wi_prop<-dataGfin$nb.x/dataGfin$nb.y*dataGfin$wi   #new 2022
# check suffixes

#Merge haul and regional weights data frames
colnames(rezF) <- c("year","area","wtC")
dataGfin$year <- as.numeric(as.character(dataGfin$year))
dataGfin <- left_join(dataGfin,rezF,by=c("year","area"))

dataGfin$WT <- dataGfin$wi_prop * dataGfin$wtC


write.taf(dataGfin, dataG_filename, dir="data/Maturity/")

print("dataG with weightings saved")
print(now()-start.tt)
