## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())

# R v 4.2
library(icesTAF)
# taf.bootstrap()

library(tidyverse)
library(ggplot2)
library(FLCore)
library(RColorBrewer)
library(gam)


mkdir("data")
mkdir("data/Natural mortality")
mkdir("data/Survey indices")
mkdir("data/SAM")
mkdir("data/SURBAR")
mkdir("output/Forecast")
mkdir("output/input data")


# Set common variables
ay <- 2025  # assessment year
ts_yrs <- 1972:ay
ages <- 0:15
pg <- 8

TAC <- 112435 # in assessment year


#Reference points - updated 2025
Fmsy <- 0.167
Fmsy_lo <- 0.155
Fmsy_hi <- 0.167
Btrig <- Bpa <- 192109
Blim <- 138250
Flim <- 0.31
Fpa <- 0.167
Fp.05 <- 0.167

fn.prefix <- paste0("had.27.46a.20 - WGNSSK ",ay)
col.pal <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[6],brewer.pal(n=6,name="Accent"))
col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])


save(list=c("ay","ts_yrs","ages","pg","TAC","Fmsy","Fmsy_lo","Fmsy_hi","Btrig","Bpa","Blim","Flim","Fpa","Fp.05",
"col.pal","col.pal9","fn.prefix"),file="data/init.RData")

options(digits=15)

sourceDir("boot/software/utilities/")

# directories
mkdir(paste0("data/FLR files - WGNSSK ",ay))

input.flr <- paste0("boot/data/FLR files - WGNSSK ",ay-1,"/") # last year's FLR files
output.flr <- paste0("data/FLR files - WGNSSK ",ay,"/") # where to save this year's FLR files
catch.flr <- paste0("boot/data/FLR files - ",ay-1," catch data/") # this year's FLR files for catch wt and num. Manually updated from IC extraction
sam.dir <- "data/SAM/" # where to save SAM files
surbar.dir <- "data/SURBAR/" # where to save surbar files
forecast.dir <-"output/Forecast/" # where to save forecast data

# stock description
st.desc <- paste0("Haddock in the Northern Shelf (had.27.46a20) (WGNSSK ",ay,"): ")


# natural mortality -------------------------------------------------------####

# read in csv. These are copy/pasted SMS outputs from their TAF repo for 2023
#https://github.com/ices-eg/wg_WGSAM/blob/master/NorthSeaKeyRun_2023/Input_Output/Output/Tables/_tab_M1M2.txt

# SMS
sms <- read.csv("boot/data/WGSAM_NS_haddock_M1M2.csv")

colnames(sms)[1] <- "Year"
colnames(sms)[2] <- "Quarter"
colnames(sms)[3:13] <- 0:10
sms$Year <- sort(rep(1974:2022,4))

# add up over quarter
tmp <- pivot_longer(sms,cols=c(as.character(0:10)),names_to="Age",values_to="M")
tmp$Age <- as.numeric(tmp$Age)
tmp <- tmp %>% group_by(Year,Age) %>% dplyr::summarise(M=sum(M,na.rm=T))

# export annual values
nm <- pivot_wider(tmp,id_cols="Year",names_from="Age",values_from="M")
write.csv(nm,file="data/Natural mortality/had.27.46a20 - annual natural mortality from WGSAM 2023.csv",row.names=F)

# Smooth for use with some assessment models 
nm_smoothed <- as.data.frame(nm)

for (i in 0:10){
  g<-gam(nm_smoothed[,as.character(i)]~s(Year,3),nm,family=gaussian)
  s<-predict.Gam(g)
  nm_smoothed[,as.character(i)] <- s
}

# export smoothed annual values
nm_smoothed <- round(nm_smoothed,4)
write.csv(nm_smoothed,file="data/Natural mortality/had.27.46a20 - annual smoothed natural mortality from WGSAM 2023.csv",row.names=F)


# maturity ogive ---------------------------------------------------####

# The analysis of survey data to derive time-varying maturity ogives can be found in the 2025_had.27.46a20_data repo
file.copy(from = "boot/data/Maturity", to="data", overwrite = TRUE, recursive = TRUE)

# survey data -------------------------------------------------------####

# # The analysis/modelling of survey data to derive the indices can be found in the 2025_had.27.46a20_data repo

# make FLR input files
q1 <- read.table("boot/data/Survey indices/survey-haddock-Q1-1-8plus.dat",skip=4,header=F)
q1[,1] <- 1983:ay
names(q1) <- c("Year",1:7,"8+")

q3q4 <- read.table("boot/data/Survey indices/survey-haddock-Q3Q4-0-8plus.dat",skip=4,header=F)
q3q4[,1] <- 1991:(ay-1)
names(q3q4) <- c("Year",0:7,"8+")

# q1 - ages 1-8, 8 is plus group
idx.q1 <- FLIndex(index = FLQuant(t(q1[,c(1:7,"8+")]),dimnames=list(age=1:8,year=q1$Year)))
idx.q1@effort <- FLQuant(1,dimnames=list(age="all",year=q1$Year))
idx.q1@name <- "delta-GAM NS-WC Q1"
idx.q1@desc <- paste0(st.desc,"survey indices")
range(idx.q1)["startf"] <- 0
range(idx.q1)["endf"] <- 0.25

# q3q4 - ages 0-8, 8 is plus group
idx.q3q4 <- FLIndex(index=FLQuant(t(q3q4[,c(0:7,"8+")]),dimnames=list(age=0:8,year=q3q4$Year)))
idx.q3q4@effort <- FLQuant(1,dimnames=list(age="all",year=q3q4$Year))
idx.q3q4@name <- "delta-GAM NS-WC Q3+Q4"
idx.q3q4@desc <- paste0(st.desc,"survey indices")
range(idx.q3q4)["startf"] <- 0.5
range(idx.q3q4)["endf"] <- 1

#### save out FLR files for indices
writeIndexVPA(idx.q1,paste0(output.flr,"nor_had_nswcq1.txt"))
writeIndexVPA(idx.q3q4,paste0(output.flr,"nor_had_nswcq3q4.txt"))

# all indices
x.idx <- FLIndices(idx.q1,idx.q3q4)
x.idx@desc <- paste0(st.desc," survey indices")
writeIndicesVPA(x.idx,paste0(output.flr,"nor_had_ef_q1_q3q4.txt"))

save(x.idx,file="data/indices.RData")



# make CV files for survey indices -------------------------------------------

# Q1
dat <- read.table("boot/data/Survey indices/survey-haddock-Q1-1-8plus.dat", header = FALSE, skip = 4)
datu <- read.table("boot/data/Survey indices/survey-haddock-Q1-1-8plus_hi.dat", header = FALSE, skip = 4)
datl <- read.table("boot/data/Survey indices/survey-haddock-Q1-1-8plus_lo.dat", header = FALSE, skip = 4)

# Inspect
fyear <- ay
for(i in 2:ncol(dat)){
  print(i)
  plot(1983:fyear, dat[,i], 
       type = "n", 
       ylim = c(0, max(datu[,i])),
       xlab = "Year", 
       ylab = "")
  polygon(c(1983:fyear, rev(1983:fyear)),
          c(datu[,i], rev(datl[,i])),
          col = "#dadada", border = "#dadada")
  lines(1983:fyear, dat[,i], lwd = 2)
}

# Calculate
sdI <- (datu - datl)/4
sdI[,1] <- 1
varI <- sdI^2

CVI <- (log(datu) - log(datl))/4
CVI[,1] <- 1

# Visualize
set.seed(1337)
hist(rnorm(1e3, dat[1, 9], sdI[1, 9]),
     main = "", xlab = "",
     breaks = 50)

# Export
filePath <- "data/Survey indices/survey-haddock-Q1-1-8plus_var.dat"
fileConn <- file(filePath)
writeLines(c("Q1 Index variances",
             "666",
             paste0("1983 ",fyear),
             #  "1 1  0.1145415 0.1145415",
             "1  8",
             "1"), fileConn)
close(fileConn)
write.table(round(varI,4), filePath,
            row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE, sep="\t")

filePath <- "data/Survey indices/survey-haddock-Q1-1-8plus_CV.dat"
fileConn <- file(filePath)
writeLines(c("Q1 Index CV",
             "666",
             paste0("1983 ",fyear),
             #      "1 1  0.1145415 0.1145415",
             "1  8",
             "1"), fileConn)
close(fileConn)
write.table(round(CVI,4), filePath,
            row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE, sep="\t")

graphics.off()

# Q3Q4 ------
dat <- read.table("boot/data/Survey indices/survey-haddock-Q3Q4-0-8plus.dat", header = FALSE, skip = 4)
datu <- read.table("boot/data/Survey indices/survey-haddock-Q3Q4-0-8plus_hi.dat", header = FALSE, skip = 4)
datl <- read.table("boot/data/Survey indices/survey-haddock-Q3Q4-0-8plus_lo.dat", header = FALSE, skip = 4)

# Inspect
fyear <- ay-1
for(i in 2:ncol(dat)){
  print(i)
  plot(1991:fyear, dat[,i], 
       type = "n", 
       ylim = c(0, max(datu[,i])),
       xlab = "Year", 
       ylab = "")
  polygon(c(1991:fyear, rev(1991:fyear)),
          c(datu[,i], rev(datl[,i])),
          col = "#dadada", border = "#dadada")
  lines(1991:fyear, dat[,i], lwd = 2)
}

# Calculate
sdI <- (datu - datl)/4
sdI[,1] <- 1
varI <- sdI^2

CVI <- (log(datu) - log(datl))/4
CVI[,1] <- 1

# Visualize
set.seed(1337)
hist(rnorm(1e3, dat[1, 9], sdI[1, 9]),
     main = "", xlab = "",
     breaks = 50)

# Export
filePath <- "data/Survey indices/survey-haddock-Q3Q4-0-8plus_var.dat"
fileConn <- file(filePath)
writeLines(c("Q3Q4 Index variances",
             "666",
             paste0("1991 ",fyear),
             #  "1 1  0.1145415 0.1145415",
             "0  8",
             "1"), fileConn)
close(fileConn)
write.table(round(varI,4), filePath,
            row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE, sep="\t")

filePath <- "data/Survey indices/survey-haddock-Q3Q4-0-8plus_CV.dat"
fileConn <- file(filePath)
writeLines(c("Q3Q4 Index CV",
             "666",
             paste0("1991 ",fyear),
             #      "1 1  0.1145415 0.1145415",
             "0  8",
             "1"), fileConn)
close(fileConn)
write.table(round(CVI,4), filePath,
            row.names = FALSE, col.names = FALSE, append = TRUE, quote = FALSE, sep="\t")

graphics.off()

# update FLR files with biols ---------------------------------------------------------

## natural mortality
nm <- read.csv("data/Natural mortality/had.27.46a20 - annual smoothed natural mortality from WGSAM 2023.csv")
names(nm) <- c("Year",0:10)
yrs <- nm$Year
rownames(nm) <- nm$Year
nm <- nm[,as.character(0:pg)]

flq <- FLQuant(t(as.matrix(nm)),dimnames=list(year=yrs,age=0:pg))
flq <- FLCore::expand(flq,age=ages)
flq[ac((pg+1):max(ages)),] <- flq[ac(pg),]

stock.data <- window(FLStock(m=flq),end=ay)
stock.data <- window(stock.data,start=1965,end=ay)
stock.data@m[,ac(1965:1973)] <- stock.data@m[,ac(1974)]
stock.data@m[,ac(2022:(ay))] <- stock.data@m[,ac(2022)] # last data provided in 2022.
units(stock.data@m) <- "NA"

## maturity
mat <- read.csv("data/Maturity/had.27.46a20 - Maturity ogive.csv") 
names(mat) <- c("Year",0:8)

stock.data@mat[] <- 1
stock.data@mat[ac(0:pg),ac(1972:(ay))] <- FLQuant(t(mat[mat$Year >1971, ac(0:pg)]),dimnames=list(age=0:pg,year=mat$Year[mat$Year >1971]))
stock.data@mat[,ac(1965:1971)] <- stock.data@mat[,ac(1972)]
units(stock.data@mat) <- "NA"

# Repeat ay-1 values on ay even though ay data are available as that is what we did at the benchmark but probably shouldn't have!
stock.data@mat[,ac(ay)] <- stock.data@mat[,ac(ay-1)]

## write out FLR files
stock.data@name <- st.desc

# nat mort
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_nm.txt"), slot.="m", desc.="natural mortality")
# mat
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_mo.txt"), slot.="mat", desc.="maturity-at-age ogive")
# pf
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_pf.txt"), slot.="harvest.spwn", desc.="proportion of F before spawning") 
# pm
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_pm.txt"), slot.="m.spwn", desc.="proportion of M before spawning") 

# FLR files for catches ---------------------------------------------------------

# read in FLR files
# data exported from InterCatch are already added to these FLR files in the bootstrap/data directory

ln <- readVPAFile(paste0(catch.flr,"/nor_had_cn_lan.txt"))
dn <- readVPAFile(paste0(catch.flr,"/nor_had_cn_dis.txt"))
cn <- readVPAFile(paste0(catch.flr,"/nor_had_cn.txt")) 
bmsn <- readVPAFile(paste0(catch.flr,"/nor_had_bmsn.txt"))
ibcn <- readVPAFile(paste0(catch.flr,"/nor_had_byn.txt"))

lw <- readVPAFile(paste0(catch.flr,"/nor_had_cw_lan.txt"))
dw <- readVPAFile(paste0(catch.flr,"/nor_had_cw_dis.txt"))
cw <- readVPAFile(paste0(catch.flr,"/nor_had_cw.txt")) 
bmsw <- readVPAFile(paste0(catch.flr,"/nor_had_bmsw.txt"))
ibcw <- readVPAFile(paste0(catch.flr,"/nor_had_byw.txt"))

lt <- readVPAFile(paste0(catch.flr,"/nor_had_ca_lan.txt"))
dt <- readVPAFile(paste0(catch.flr,"/nor_had_ca_dis.txt"))
ct <- readVPAFile(paste0(catch.flr,"/nor_had_ca.txt")) 
bmst <- readVPAFile(paste0(catch.flr,"/nor_had_bms.txt"))
ibct <- readVPAFile(paste0(catch.flr,"/nor_had_by.txt"))

# check consistency - should be 0
sum(round(cn-(ln+dn+bmsn+ibcn),2))
tmp <-((ln*lw+dn*dw+bmsn*bmsw+ibcn*ibcw)/c(ln+dn+bmsn+ibcn))
tmp[is.na(tmp)] <- 0
sum(round(cw-tmp,2))

# add to stock.data object
stock.data@landings[,ac(1965:(ay-1))] <-lt
stock.data@discards[,ac(1965:(ay-1))] <-dt
stock.data@catch[,ac(1965:(ay-1))] <-ct
units(stock.data@landings) <- units(stock.data@discards) <- units(stock.data@catch) <- "tonnes" # check

stock.data@landings.n[,ac(1965:(ay-1))] <-ln
stock.data@discards.n[,ac(1965:(ay-1))] <-dn
stock.data@catch.n[,ac(1965:(ay-1))] <-cn
units(stock.data@landings.n) <- units(stock.data@discards.n) <- units(stock.data@catch.n) <- "thousands" # check

stock.data@landings.wt[,ac(1965:(ay-1))] <-lw
stock.data@discards.wt[,ac(1965:(ay-1))] <-dw
stock.data@catch.wt[,ac(1965:(ay-1))] <-cw
units(stock.data@landings.wt) <- units(stock.data@discards.wt) <- units(stock.data@catch.wt) <- "kg"

units(bmsn) <- units(ibcn) <- "thousands" # check
units(bmsw) <- units(ibcw) <- "kg" 
units(bmst) <- units(ibct) <- "tonnes" 


## write out FLR files
# ln 
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cn_lan.txt"), slot.="landings.n", desc.="landings-at-age") 
# dn
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cn_dis.txt"), slot.="discards.n", desc.="discards-at-age") 
# cn
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cn.txt"), slot.="catch.n", desc.="catch-at-age") 
# lw
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cw_lan.txt"), slot.="landings.wt", desc.="landings weight--at-age") 
# dw
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cw_dis.txt"), slot.="discards.wt", desc.="discards weight-at-age") 
# cw
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_cw.txt"), slot.="catch.wt", desc.="catch weight-at-age") 

# lt 
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_ca_lan.txt"), slot.="landings", desc.="total landings") 
# dt
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_ca_dis.txt"), slot.="discards", desc.="total discards") 
# ct
writeVPAFile(FLStock.=window(stock.data,end=ay-1), file.=paste0(output.flr,"nor_had_ca.txt"), slot.="catch", desc.="total catch") 

# bmsn
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_bmsn.txt"),slot.="landings.n",desc.="BMS landings-at-age",
             obj.=bmsn,name.= st.desc) 
# ibcn
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_byn.txt"),slot.="landings.n",desc.="IBC landings-at-age",
             obj.=ibcn,name.= st.desc) 
# bmsw
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_bmsw.txt"),slot.="landings.wt",desc.="BMS landings weight-at-age",
             obj.=bmsw,name.= st.desc) 
# ibcw
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_byw.txt"),slot.="landings.wt",desc.="IBC landings weight-at-age",
             obj.=ibcw,name.= st.desc) 
# bmst
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_bms.txt"),slot.="landings",desc.="BMS yield",
             obj.=bmst,name.= st.desc) 
# ibct
writeVPAFile(FLStock.=NULL,file.=paste0(output.flr,"nor_had_by.txt"),slot.="landings",desc.="IBC yield",
             obj.=ibct,name.= st.desc) 


# Stock weights ---------------------------------------------------------

# apply survey derived correction factors to catch mean weights
corr.factor <- read.csv("boot/data/had.27.46a20 - Mean_catchwt_to_stockwt_correction_factors.csv")
names(corr.factor) <- ac(0:pg)
corr.factor <- matrix(rep(corr.factor,length(1965:(ay))),nrow=9,dimnames=list(0:pg,1965:(ay)))
cf <- FLQuant(corr.factor,dimnames=list(age=0:pg,year=1965:(ay)))

# calc stock weights
cw <- stock.data@catch.wt # catch weights
cn <- stock.data@catch.n # catch numbers
sw_pg <- quantSums(trim(cw*cn,age=pg:15))/quantSums(trim(cn,age=pg:15))
sw <- trim(cw,age=0:pg)
sw[ac(pg),] <- sw_pg
sw <- sw*cf
sw <- FLCore::expand(sw,age=ages)
sw[ac(9:15),] <- sw[ac(pg),]

# add to stock data
sw[,ac(ay)] <- sw[,ac(ay-1)] # repeat 2022 values in 2023
stock.data@stock.wt <- round(sw,3)
units(stock.data@stock.wt) <- "kg"

writeVPAFile(FLStock.=stock.data, file.=paste0(output.flr,"nor_had_sw.txt"), slot.="stock.wt", desc.="stock weight-at-age")

# Make SAM files ---------------------------------------------------------

# copy across files that dont need editing
file.copy(from = file.path(output.flr,"nor_had_cn.txt"), to = file.path(sam.dir,"cn.dat"),overwrite=T)
file.copy(from = file.path(output.flr,"nor_had_cw.txt"), to = file.path(sam.dir,"cw.dat"),overwrite=T)
file.copy(from = file.path(output.flr,"nor_had_cw_lan.txt"), to = file.path(sam.dir,"lw.dat"),overwrite=T)
file.copy(from = file.path(output.flr,"nor_had_ef_q1_q3q4.txt"), to = file.path(sam.dir,"survey.dat"),overwrite=T)

# copy survey CV files
file.copy(from = file.path("data/Survey indices/survey-haddock-Q1-1-8plus_CV.dat"), to = file.path(sam.dir,"survey-haddock-Q1-1-8plus_CV.dat"),overwrite=T)
file.copy(from = file.path("data/Survey indices/survey-haddock-Q3Q4-0-8plus_CV.dat"), to = file.path(sam.dir,"survey-haddock-Q3Q4-0-8plus_CV.dat"),overwrite=T)

#mat
writeVPAFile(FLStock.=stock.data, file.=paste0(sam.dir,"mo.dat"), slot.="mat", desc.="maturity-at-age ogive") 
#nat mort
writeVPAFile(FLStock.=stock.data, file.=paste0(sam.dir,"nm.dat"), slot.="m", desc.="natural mortality") 
#stock weights
writeVPAFile(FLStock.=stock.data, file.=paste0(sam.dir,"sw.dat"), slot.="stock.wt", desc.="stock weight-at-age") 
#pf
writeVPAFile(FLStock.=stock.data, file.=paste0(sam.dir,"pf.dat"), slot.="harvest.spwn", desc.="proportion of F before spawning") 
#pm
writeVPAFile(FLStock.=stock.data, file.=paste0(sam.dir,"pm.dat"), slot.="m.spwn", desc.="proportion of M before spawning") 

# landings fraction (L/C) = landings numbers
ln <- readVPAFile(paste0(output.flr,"nor_had_cn_lan.txt")) # landings numbers

writeVPAFile(FLStock.=NULL,file.=paste0(sam.dir,"lf.dat"),desc.="landings-at-age",slot.="landings.n",
             obj.=ln,name.= st.desc) 

# combine discards, bms and ibc
dn <- readVPAFile(paste0(output.flr,"nor_had_cn_dis.txt")) # discard numbers
byn <- readVPAFile(paste0(output.flr,"nor_had_byn.txt")) # bycatch numbers
bn <- readVPAFile(paste0(output.flr,"nor_had_bmsn.txt")) # bms numbers
dw <- readVPAFile(paste0(output.flr,"nor_had_cw_dis.txt")) # discard weights
byw <- readVPAFile(paste0(output.flr,"nor_had_byw.txt")) # bycatch weights
bw <- readVPAFile(paste0(output.flr,"nor_had_bmsw.txt")) # bms weights

dn_new <- dn+byn+bn
dw_new <- round((dn*dw+byn*byw+bn*bw)/(dn+byn+bn),3)
dw_new[is.na(dw_new)] <- 0

# check consistency - should be 0
cn <- readVPAFile(paste0(sam.dir,"cn.dat"))
cw <- readVPAFile(paste0(sam.dir,"cw.dat"))
sum(round(cn-(ln+dn+byn+bn),2))
tmp <-((ln*lw+dn_new*dw_new)/c(ln+dn_new))
tmp[is.na(tmp)] <- 0
sum(round(cw-tmp,2))

writeVPAFile(FLStock.=NULL,file.=paste0(sam.dir,"dw.dat"),desc.="discards weight-at-age",slot.="discards.wt",
             obj.=dw_new,name.= st.desc) 

writeVPAFile(FLStock.=NULL,file.=paste0(sam.dir,"nor_had_cn_dis.txt"),desc.="discards-at-age",slot.="discards.n",
             obj.=dn_new,name.= st.desc)


# Make SURBAR files ---------------------------------------------------------

# need to trim down to first survey year (1983)

# ages 1-7
#mat
writeVPAFile(FLStock.=window(trim(stock.data,age=1:7),start=1983), file.=paste0(surbar.dir,"nosh_had_mo.dat"), slot.="mat", desc.="maturity-at-age ogive") 
#stock weights
writeVPAFile(FLStock.=window(trim(stock.data,age=1:7),start=1983), file.=paste0(surbar.dir,"nosh_had_sw.dat"), slot.="stock.wt", desc.="stock weight-at-age") 

# surveys
# ages 1-7
x.idx.trim <- FLIndices(trim(idx.q1,age=1:7),trim(idx.q3q4,age=1:7))
x.idx.trim@desc <- paste0(st.desc,"survey indices")
writeIndicesVPA(x.idx.trim,paste0(surbar.dir,"nosh_had_ibts.dat"))


# Forecast weights ---------------------------------------------------------

# cohort growth modelling - Jaworski et al 2011
years <- 1965:(ay-1)

# landings
ca.wt <- read.table(paste0(output.flr,"nor_had_cw_lan.txt"),skip = 5, header=F)
ca.n <- read.table(paste0(output.flr,"nor_had_cn_lan.txt"),skip = 5, header=F)
names(ca.wt) <- names(ca.n) <- as.character(ages)
row.names(ca.wt) <- row.names(ca.wt) <- years


# calc forecast wts
ca.wt.mod <-jaworski.mod.wts(ca.wt=ca.wt,ages=ages,years=years,type="landings",output.dir=forecast.dir)
lan.n <- ca.n
lan.mod.wt <- ca.wt.mod

#save output
write.table(ca.wt.mod,file=paste0(forecast.dir,"/had.27.46a20 - Jarworski model results - landings-at-age.txt"),sep="\t",quote=F)

# calc plus group
names(ca.n)<-as.character(ages)
row.names(ca.n)<-years

# weight next 3 years (modelled weight) by numbers at age over the last 3 years of data
ca<-ca.n[c((dim(ca.n)[1]-2):(dim(ca.n)[1])),]
wt<-ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),]
mean.ca <- rbind(colMeans(ca,na.rm=T),colMeans(ca,na.rm=T),colMeans(ca,na.rm=T))
cawt<-mean.ca*wt
wt.plgr<-rowSums(cawt[,as.character(c(pg:colnames(cawt)[dim(cawt)[2]]))])/
  rowSums(mean.ca[,as.character(c(pg:colnames(mean.ca)[dim(mean.ca)[2]]))])

# bind results together
min.age<-min(as.numeric(colnames(ca.wt.mod)))
frcst.wts<-cbind(ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),as.character(c(min.age:(pg-1)))],wt.plgr)
names(frcst.wts)<-c(c(min.age:(pg-1)),paste(pg,"+",sep=""))

# round to 3dp
frcst.wts<-round(frcst.wts,digits=3)
frcst.wts["0"][is.na(frcst.wts["0"])] <- 0

#save output
write.table(frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - landings-at-age.txt"),sep="\t",quote=F)


# discards and bms and ibc
ca.wt <- read.table(paste0(output.flr,"nor_had_cw_dis.txt"),skip = 5, header=F)
ca.n <- read.table(paste0(output.flr,"nor_had_cn_dis.txt"),skip = 5, header=F)
ca.wt1 <- read.table(paste0(output.flr,"nor_had_bmsw.txt"),skip = 5, header=F)
ca.n1 <- read.table(paste0(output.flr,"nor_had_bmsn.txt"),skip = 5, header=F)
ca.wt2 <- read.table(paste0(output.flr,"nor_had_byw.txt"),skip = 5, header=F)
ca.n2 <- read.table(paste0(output.flr,"nor_had_byn.txt"),skip = 5, header=F)

# combine dis and bms
ca.wt <- (ca.n*ca.wt + ca.n1*ca.wt1 + ca.n2*ca.wt2)/(ca.n+ca.n1+ca.n2)
ca.wt[is.na(ca.wt)] <- 0
ca.n <- ca.n+ca.n1+ca.n2

names(ca.wt) <- names(ca.n) <- as.character(ages)
row.names(ca.wt) <- row.names(ca.wt) <- years

ca.wt.mod <-jaworski.mod.wts(ca.wt=ca.wt,ages=ages,years=years,type="discards",output.dir = forecast.dir)
dis.n <- ca.n
dis.mod.wt <- ca.wt.mod

#save output
write.table(ca.wt.mod,file=paste0(forecast.dir,"/had.27.46a20 - Jarworski model results - discards-at-age incl BMS and IBC.txt"),sep="\t",quote=F)

# calc plus group
names(ca.n)<-as.character(ages)
row.names(ca.n)<-years

# weight next 3 years (modelled weight) by numbers at age over the last 3 years of data
ca<-ca.n[c((dim(ca.n)[1]-2):(dim(ca.n)[1])),]
wt<-ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),]
mean.ca <- rbind(colMeans(ca,na.rm=T),colMeans(ca,na.rm=T),colMeans(ca,na.rm=T))
cawt<-mean.ca*wt
wt.plgr<-rowSums(cawt[,as.character(c(pg:colnames(cawt)[dim(cawt)[2]]))])/
  rowSums(mean.ca[,as.character(c(pg:colnames(mean.ca)[dim(mean.ca)[2]]))])

# bind results together
min.age<-min(as.numeric(colnames(ca.wt.mod)))
frcst.wts<-cbind(ca.wt.mod[c((dim(ca.wt.mod)[1]-2):(dim(ca.wt.mod)[1])),as.character(c(min.age:(pg-1)))],wt.plgr)
names(frcst.wts)<-c(c(min.age:(pg-1)),paste(pg,"+",sep=""))

# round to 3dp
frcst.wts<-round(frcst.wts,digits=3)

#save output
write.table(frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt"),sep="\t",quote=F)


# catch
ca.n <- read.table(paste0(output.flr,"nor_had_cn_lan.txt"),skip = 5, header=F)
ca.n1 <- read.table(paste0(output.flr,"nor_had_cn_dis.txt"),skip = 5, header=F)
ca.n2 <- read.table(paste0(output.flr,"nor_had_byn.txt"),skip = 5, header=F)
ca.n3 <- read.table(paste0(output.flr,"nor_had_bmsn.txt"),skip = 5, header=F)

# make plus group
ca.n[,9] <- rowSums(ca.n[,9:16],na.rm=T)
ca.n1[,9] <- rowSums(ca.n1[,9:16],na.rm=T)
ca.n2[,9] <- rowSums(ca.n2[,9:16],na.rm=T)
ca.n3[,9] <- rowSums(ca.n3[,9:16],na.rm=T)
ca.n <- ca.n[,1:9]
ca.n1 <- ca.n1[,1:9]
ca.n2 <- ca.n2[,1:9]
ca.n3 <- ca.n3[,1:9]

#calc prop
prop.lan <- colMeans((ca.n/(ca.n+ca.n1+ca.n2+ca.n3))[(nrow(ca.n)-2):nrow(ca.n),],na.rm=T)
prop.disbmsby <- colMeans(((ca.n1+ca.n2+ca.n3)/(ca.n+ca.n1+ca.n2+ca.n3))[(nrow(ca.n)-2):nrow(ca.n),],na.rm=T)

#check
prop.lan+prop.disbmsby # should be 1 for each age
prop.lan <- rbind(prop.lan,prop.lan,prop.lan)
prop.disbmsby <- rbind(prop.disbmsby,prop.disbmsby,prop.disbmsby)

# get wts
lan.frcst.wts <- read.table(file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - landings-at-age.txt"))
dis.frcst.wts <- read.table(file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - discards-at-age incl BMS and IBC.txt"))

# check this is only the first age
lan.frcst.wts[is.na(lan.frcst.wts)]<-0
dis.frcst.wts[is.na(dis.frcst.wts)]<-0

cat.frcst.wts <- round((prop.lan*lan.frcst.wts+
                          prop.disbmsby*dis.frcst.wts)/(prop.lan+prop.disbmsby),3)
colnames(cat.frcst.wts) <- c(c(0:(pg-1)),paste(pg,"+",sep=""))
write.table(cat.frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - catch-at-age.txt"),sep="\t",quote=F)

#stock wts - apply correction factor
cf <- read.csv("boot/data/had.27.46a20 - Mean_catchwt_to_stockwt_correction_factors.csv")
stk.frcst.wts <- round(rbind(cf,cf,cf) * cat.frcst.wts,3)
colnames(stk.frcst.wts) <- c(c(0:(pg-1)),paste(pg,"+",sep=""))
row.names(stk.frcst.wts) <- (c(last(years):(last(years)+2)))+1
write.table(stk.frcst.wts,file=paste0(forecast.dir,"/had.27.46a20 - Forecast weights - stock-at-age.txt"),sep="\t",quote=F)


# make FLR objects (data only) ---------------------------------------------------------

stock.data65 <- stock.data # full time series back to 1965
stock.data <- window(stock.data,start=1972) # assessment time series starts in 1972

# plus group
stock.data.pg <- setPlusGroup(stock.data,plusgroup=8)
stock.data65.pg <- setPlusGroup(stock.data65,plusgroup=8)

# overwrite some slots
stock.data.pg@stock.wt["8",] <- stock.data@stock.wt["8",] # stock weight is the set the same for ages 8-15
stock.data65.pg@stock.wt["8",] <- stock.data65@stock.wt["8",] # stock weight is the set the same for ages 8-15

# BMS and IBC 
bmsn.pg <- setPlusGroup(bmsn,8)
ibcn.pg <- setPlusGroup(ibcn,8)

bmsw.pg <- trim(bmsw,age=0:8)
bmsw.pg["8",] <- quantSums(bmsn[ac(8:15),]*bmsw[ac(8:15),],na.rm=T)/quantSums(bmsn[ac(8:15),],na.rm=T)
is.na(bmsw.pg["8",]) <- 0
ibcw.pg <- trim(ibcw,age=0:8)
ibcw.pg["8",] <- quantSums(ibcn[ac(8:15),]*ibcw[ac(8:15),],na.rm=T)/quantSums(ibcn[ac(8:15),],na.rm=T)
is.na(ibcw.pg["8",]) <- 0


save(list=c("stock.data65","stock.data65.pg","stock.data","bmsn","bmsw","ibcn","ibcw",
            "stock.data.pg","bmsn.pg","bmsw.pg","ibcn.pg","ibcw.pg"),file="data/stockData.RData")


