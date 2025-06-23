## Extract results of interest, write TAF output tables

## Before:
## After:

rm(list=ls())
graphics.off()

library(icesTAF)
detach.packages()
library(icesTAF)

mkdir("output")


# Survey indices ----------------------------------------------------------------

library(surveyIndex)
library(maptools)
library(ggplot2)
library(tidyverse)
library(icesAdvice)


mkdir("output/indices")

source("boot/software/utility_functions_indices.R")
output.dir <- "output/indices/"

source("output_01_Q1.R")
source("output_02_Q3Q4.R")
source("output_02a_Indices_cohort_map.R")

# Get external consistency
load("model/SImodel_Q1.RData")
SIQ1 <- SI
load("model/SImodel_Q3Q4.RData")
SIQ3Q4 <- SI

sink(paste(output.dir,"Q1_Q3Q4_external_cons.txt"))
externalCons(SIQ1$idx[as.character(yearsQ3Q4),], SIQ3Q4$idx[,as.character(agesQ1)], do.plot = FALSE)
sink()


# Maturity, weights and lengths --------------------------------------------------------------------

rm(list=ls())
graphics.off()

detach.packages()
library(icesTAF)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)


mkdir("output/Maturity")
output.dir <- "output/Maturity/"

source("output_03_maturity.R")

mkdir("output/weights and lengths")
output.dir <- "output/weights and lengths/"

source("output_04_weights_and_lengths.R")
