## Preprocess data, write TAF data tables

## Before:
## After:

rm(list=ls())
graphics.off()

library(icesTAF)
library(RColorBrewer)

mkdir("data")

options(digits=15)
options(download.file.method="curl")

# Set common variables
ay <- 2025  # assessment year

##### Species specific parameters for indices
cmSize <- 1
genus <- "Melanogrammus"
bfamily <- "aeglefinus"

agesQ1 <- 1:8
yearsQ1 <- c(1983:ay) 

agesQ3Q4 <- 0:8
yearsQ3Q4 <- c(1991:(ay-1)) 

mc.cores <- 1
runRetro <- F

# colour palettes
col.pal <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=12,name="Set3"))
col.pal.extra <- c(col.pal,brewer.pal(n = 9, name = "Set1")[c(1,2,4,5,6,8,9)],brewer.pal(n=8,name="Pastel1")[c(1,2,4,5,7)])


save(list=c("ay","cmSize","genus","bfamily","agesQ1","agesQ3Q4","yearsQ1","yearsQ3Q4","mc.cores","runRetro",
            "col.pal","col.pal.extra"),
     file="data/data_init.RData")


# Survey indices ----------------------------------------------------------------

# For indices
library(DATRAS)
library(maptools)
library(sp)
library(surveyIndex)
library(lattice)
library(ggplot2)
library(dplyr)

source("data_01_Q1.R")
source("data_02_Q3Q4.R")

# Biologicals ----------------------------------------------

rm(list=ls())
graphics.off()

detach.packages()
library(icesTAF)
library(DATRAS)
library(nlme)
library(tidyverse)
library(mapplots)
library(mgcv)


mkdir("data/Maturity")
mkdir("data/Weights and lengths")

#remember to set seed
source("data_03_preprocess_DATRAS.R")

rm(list=ls())
graphics.off()
source("data_04_maturity.R")

rm(list=ls())
graphics.off()
source("data_05_weights_and_lengths.R")
