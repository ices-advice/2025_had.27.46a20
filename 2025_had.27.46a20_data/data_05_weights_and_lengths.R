######################################################
# Haddock stock weights-at-age in the Northern Shelf #
# Author: Aaron Brazier                              
# Date created: 22/10/2021 15:07:09                  
# Last edited:  24/03/2025                   
# Version: 3                                         
######################################################                                                                                    


# FYI - this is just a generic analysis of weights and lengths
# If wanting to make sw-cw correction factors see the code from the benchmark.

#Load packages
#library(boot)  
#library(svglite) 
#library(ggplot2)
#library(grid)
#library(gridExtra)
#library(gam)
#library(mapplots)
#library(maptools)
#library(MASS)


#Set workspace parameters
load("data/data_init.RData")
fyear <- ay
years   <- 2000:fyear
ages    <- agesQ1
sexes <- c("M", "F", "U")

# Step 1. load pre-processed survey data -----------------------------------

load("data/pre-processed survey data.RData")

# Step 2. Data manipulation ---------------------------------------------------

#Subset to Q1 haddock data with only valid hauls and remove missing weights
haddock <- subset(haddock, !is.na(IndWgt))

# Filter
haddock <- subset(haddock, Year %in% years)
haddock <- subset(haddock, Sex %in% sexes)
haddock <- subset(haddock, Age %in% ages) 


# Step 3. Create SMALK from CA data --------------------------------------
#Create SMALK from CA data

#Total numbers per haul
haddock[["HH"]]$nbL <- rowSums(haddock[["HH"]]$N)
smalk <- haddock[["CA"]]

#Simplify SMALK dataset and maturity rating
smalk$year    <- smalk$Year
smalk$length  <- smalk$LngtCm
smalk$age     <- smalk$Age
smalk$strec    <- smalk$AreaCode
smalk$sex     <- smalk$Sex
smalk$mat     <- smalk$Maturity
smalk$indwgt  <- smalk$IndWgt
smalk$nb      <- smalk$NoAtALK

smalk  <- smalk[,c("haul.id","year","length","age","strec","sex","mat","indwgt","nb")]

#Remove unnecessary variables from SMALK data frame
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

write.taf(catr, "data/Weights and lengths/Catch_Rate_By_Area.csv")


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

rez$proparea <- NULL
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

write.taf(rezF, "data/Weights and lengths/rezF_stockweights.csv")

rezF <- rezF[,c(1:2,6)]
rezFT <- rezF %>% group_by(Year) %>% summarise(tot=sum(nbL_2))
rezF <- left_join(rezF,rezFT)

rezF$wtC <- rezF$nbL_2 / rezF$tot

rezF <- rezF[,c(1,2,5)]

# Step 6. Generate final dataset ----------------------------------------------------

#Generate the final dataset we will be working with: 

dataG <- smalk %>% group_by(haul.id,age,length) %>% 
  summarise(nb=sum(nb), weight=mean(indwgt), year=unique(year), area=unique(area))

# Step 7. Generate weighting factors ----------####
#USE CEFAS weighting FACTOR as wi = ma x rg/Ra

#Generate intermediate data set for Length
wL <- as.data.frame(haddock[["HH"]]$N)
wL$haul.id <- haddock[["HH"]]$haul.id
rownames(wL) <- c()

#produce the rg raising factor each fish
wL1 <- droplevels(subset(wL,haul.id %in% dataG$haul.id))
wL1$length <- as.numeric(sub('.([^,]+),.*', '\\1', wL1$sizeGroup)) 

dataG <- left_join(dataG,wL1,by=c("haul.id","length"))  
dataG <- dataG %>% rowwise() %>% mutate(rg = max(nb,Freq)/nb)
dataG <- dataG %>% select(!c(sizeGroup,Freq))

#produce the Ra raising factor
tmp <- dataG %>% group_by(haul.id,age) %>% summarise(Ra=sum(rg))
dataG <- left_join(dataG,tmp)

#produce the wi raising factor
tmp <- dataG %>% group_by(haul.id,age) %>% summarise(nbT=sum(nb))
dataG <- left_join(dataG,tmp,by=c("haul.id","age"))
dataG <- dataG %>% rowwise() %>% mutate(wi=nbT*rg/Ra)
dataG <- dataG %>% select(!c(nbT))

# combine
colnames(rezF) <- c("year","area","wtC")
dataG$year <- as.numeric(as.character(dataG$year))
dataG <- left_join(dataG,rezF,by=c("year","area"))
dataG$WT <- dataG$wi*dataG$wtC

write.taf(dataG,"data/Weights and lengths/dataG_mean_weights_lengths.csv")


# get mean weights at age with weighting factor for combined stock area ----------------------------------
#combined
dataG <- read.table("data/Weights and lengths/dataG_mean_weights_lengths.csv",sep=","  ,header=T)

d <- dataG
d$new_nb <- d$WT #New weighted numbers
d$prod <- d$weight*d$new_nb 

dd <- d %>% group_by(year,age) %>% summarise(nb = sum(new_nb), prod=sum(prod))

dd$weight <- dd$prod/dd$nb #Get average weight

write.taf(dd[,c("year","age","weight")],"data/Weights and lengths/mean_weights_combinedRegions.csv")

# get mean length at age with weighting factor for combined stock area ----------------------------------

#combined length at age
dataG <- read.table("data/Weights and lengths/dataG_mean_weights_lengths.csv",sep=","  ,header=T)

d <- dataG
d$new_nb <- d$WT # new weighted numbers
d$prod <- d$length*d$new_nb 

dd <- d %>% group_by(year,age) %>% summarise(nb = sum(new_nb), prod=sum(prod))

dd$length <- dd$prod/dd$nb # get average length

write.taf(dd[,c("year","age","length")],"data/Weights and lengths/mean_length_combinedRegion.csv")


# get length distribution with weighting factor for combined stock area ----------------------------------

#Length frequency
dataG <- read.table("data/Weights and lengths/dataG_mean_weights_lengths.csv",sep=","  ,header=T)

d <- dataG
d$new_nb <- d$WT # new weighted numbers

dd <- d %>% group_by(year,age,length) %>% summarise(nb = sum(new_nb))

write.taf(dd[,],"data/Weights and lengths/mean_length_freq_combinedRegion.csv")

# get mean weights at age with weighting factor for each region ----------------------------------

#Individual regions
dataG <- read.table("data/Weights and lengths/dataG_mean_weights_lengths.csv",sep=","  ,header=T)

d <- dataG
d$new_nb <- d$WT # new weighted numbers
d$prod <- d$weight*d$new_nb 

dd <- d %>% group_by(year,age,area) %>% summarise(nb=sum(new_nb), prod=sum(prod))
dd$weight <- dd$prod/dd$nb # get average weight

write.taf(dd[,c("year","age","area","weight")],"data/Weights and lengths/mean_weight_byRegion.csv", row.names=T)

