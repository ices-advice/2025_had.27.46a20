## Run analysis, write model results

## Before:
## After:

rm(list=ls())
graphics.off()

library(icesTAF)
detach.packages()
library(icesTAF)

mkdir("model")

# Survey indices ----------------------------------------------------------------

library(surveyIndex)

#remember to set seed
source("model_01_Q1.R")
source("model_02_Q3Q4.R")

# Maturity ogive ---------------------------------------------------------------

rm(list=ls())
graphics.off()

library(icesTAF)
detach.packages()
library(icesTAF)
#library(DATRAS)
library(reshape2)
library(boot)
library(gam)
#library(mgcv)

source("boot/software/utility_functions_maturity.R")
#remember to set seed
source("model_03_maturity.R") # calc ogive and retro

