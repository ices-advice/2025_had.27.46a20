## Run analysis, write model results

## Before:
## After:

rm(list=ls())

library(icesTAF)
library(stockassessment)
library(icesAdvice)
library(RColorBrewer)
library(ggplot2)

# for surbar
library(minpack.lm)
library(MASS)
library(lattice)
library(grid)

# for ref points
library(msy)
require(FLCore)
require(FLfse)
library(data.table)
library(gplots)

mkdir("model/SAM")
mkdir("model/SURBAR")
mkdir("output/SURBAR")
mkdir("output/ref_pts")

sourceDir("boot/software/utilities/")

load("data/init.RData")

#Download SAM assessments -------------------------------------------------------------------

# Run on stockassessment.org and download here

getRuns <- function(runName, x, dest.dir){
  url1 <- paste0("https://stockassessment.org/datadisk/stockassessment/userdirs/user259/",runName,"/run/",x,".RData")
  filename1 <-paste0(dest.dir,"/",x,".RData")
  download.file(url1,filename1,method="wininet",  extra = "--no-check-certificate")
}

# this year's final run
runName <- "NShaddock_WGNSSK2025_Run1"
mkdir(paste0("model/SAM/",runName))

lapply(c("model","residuals","retro","leaveout"),getRuns,dest.dir =paste0("model/SAM/",runName),runName=runName)

# last year's final run
runName <- "NShaddock_WGNSSK2024_Run2"
mkdir(paste0("model/SAM/",runName))

lapply(c("model","residuals","retro","leaveout"),getRuns,dest.dir =paste0("model/SAM/",runName),runName=runName)

# alternative run - not used
# runName <- "NShaddock_WGNSSK2025_Run3"
# mkdir(paste0("model/SAM/",runName))
# 
# lapply(c("model","residuals","retro","leaveout"),getRuns,dest.dir =paste0("model/SAM/",runName),runName=runName)

#--------------------------------------------------------------------##

source("model_surbar.R")
source("model_reference_points.R") 
source("model_forecast.R")

