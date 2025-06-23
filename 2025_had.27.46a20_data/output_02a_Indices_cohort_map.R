

rm(list=ls())
graphics.off()

library(icesTAF)
library(DATRAS)
library(surveyIndex)
library(maptools)
library(tidyverse)
library(icesAdvice)

output.dir <- "output/indices/"

# load data and model outputs ---------------------------------------------------####

load("data/data_init.RData")
load(paste0("data/dQ3Q4withALK_",ay-1,".RData"))
load(paste0("data/dQ1withALK_",ay,".RData"))

load(paste0("model/SImodel_Q1.RData"))
SI1 <- SI
grid1 <- grid
load(paste0("model/SImodel_Q3Q4.RData"))
SI34 <- SI
grid34 <- grid


# extract model results for map ----------------------------------------------------------
# get inputs

quarters <- c("Q1","Q3+Q4")
datLst <- list(dQ1,dQ3Q4)
SILst <- list(SI1,SI34)
gridLst  <- list(grid1[[3]],grid34[[3]])
### function
getSurveyMapData <-   function (SILst, datLst, quarters,gridLst) {
  
  # inputs listed by quarter (or survey index)
  names(SILst) <- names(gridLst) <- names(datLst) <- quarters
  
  # build output objects
  n <-  length(quarters)
  allyLst <- vector(length=n,mode="list") # data for mapping by year and quarter (survey)
  tmpLst <- vector(length=length(quarters),mode="list") # grid points by quarter (survey)
  
  names(allyLst) <-  names(tmpLst) <- quarters
  
  # loop through quarters (survey)
  for (q. in quarters){
    
    # get data, grid ids and SI for selected survey
    dat <- datLst[[q.]]
    myids <- gridLst[[q.]]  
    x <- SILst[[q.]]
    
    # data dimensions
    cols = 1:length(x$pModels) # ages
    
    # subset hauls and save out to output list
    tmp = subset(dat, haul.id %in% myids)
    tmpLst[[q.]] <- tmp
    
    # loop through years
 #   for (year in yearLst[[q.]]){
      
      # checks
     # if (is.null(year) || length(year) < 1) 
    #    stop("argument 'year' must be vector of length>=1")
    #  if (!all(year %in% levels(dat$Year))) 
    #    stop("invalid years selected")
      
      allAges <- NULL # output object for data from all ages
      for (a in cols){ # loop through ages
        
        #get data and transform
        ally = data.frame(val = x$gPreds2[[a]][[1]], year = as.character(levels(dat$Year)[1]))
        cc = 0
        for (y in levels(dat$Year)) {
          cc = cc + 1
          ally = rbind(ally, data.frame(val = x$gPreds2[[a]][[cc]], 
                                        year = as.character(levels(dat$Year)[cc])))
        }
     #   ally$conc = surveyIndex:::concTransform(log(ally$val))
        
        # quarter and age
        ally$quarter <- q.
        ally$age <- a
        
        # bind rows over all ages
        allAges <- rbind(allAges,ally)
      }
      # add data for all ages to output list
    
      allyLst[[q.]] <- allAges
    #}
  }
  return(list(data=allyLst,grid=tmpLst)) 
  
}

# Get data and map
toplot <- getSurveyMapData(SILst, datLst, quarters,gridLst)
df_list <- toplot$data
ally <- bind_rows(df_list)
rownames(ally) <- 1:nrow(ally)

# correct age in Q3+Q4
ally$age[ally$quarter %in% "Q3+Q4"] <- ally$age[ally$quarter %in% "Q3+Q4"]-1

# add cohort
ally$cohort <- as.numeric(ally$year)-ally$age

# Plots -------------------------------------------------------------

# Plot settings
yrs <- (ay-7):ay
qtrs <- quarters
cohorts <- c((ay-9):(ay-1))

map.cex <- 0.7
colors <- viridis(16)

xlims = range(dQ1$lon, na.rm = TRUE) # longitudes
ylims = range(dQ1$lat, na.rm = TRUE) # latitudes

# get concentrations for each year and age and quarter
ally$conc <- NA
for (q. in quarters){
  for (yy in unique(ally$year)){
    for (a in 0:9){
      idx <- which(ally$quarter %in% q. & ally$year %in% yy & ally$age %in% a)
      if(length(idx>0)){
        tmp1 <- ally[idx,]
        tmp2 <- surveyIndex:::concTransform(log(tmp1$val))
        ally$conc[idx] <- tmp2 
      }
    }
  }
}
ally$zFac <- cut(ally$conc, 0:length(colors)/length(colors)) 

# # add color scale per cohort
# ally$chCol <- NA
# for (ch in unique(ally$cohort)){
#   idx <- which(ally$cohort %in% ch)
#   tmp1 <- ally[idx,]
#   tmp2 <- surveyIndex:::concTransform(log(tmp1$val))
#   tmpzFac = cut(tmp2, 0:length(colors)/length(colors)) # reset colour by cohort
#   ally$chCol[idx] <- tmpzFac
# }

# Q1 only

png(filename=paste0(output.dir, "Survey indices - Q1 relative abundance by age and year ",min(yrs),"-",max(yrs),".png"), height=7, width=11, res=300, units="in")

mat <- matrix(1:9,nrow=9,ncol=length(yrs)) +t(matrix(seq(0,9*(length(yrs)-1),9),nrow=length(yrs),ncol=9))
layout(mat)
par(mar=c(0,0.5,1,0.5)+0.1, mgp=c(1.9, 0.8, 0))
for (yy in yrs){
  for (q. in qtrs[1]){
    for (a in 0:8){
      ally_tmp <- ally[ally$year %in% yy & ally$quarter %in% q. & ally$age %in% a,]
      tmp <- toplot$grid[[q.]]
      
      if(nrow(ally_tmp)>0){
        lab <- paste0(q.,"_",yy,":",a)
        
        #plot
        plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, 
             cex = map.cex, 
             axes = FALSE)
        box()
        title(lab, line = 0.2,cex.main=0.8)
        points(tmp$lon, y = tmp$lat, col = colors[as.numeric(ally_tmp$zFac)], 
               pch = 16, cex = map.cex)
        maps::map("worldHires", xlim = xlims, ylim = ylims, 
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
      }else{
        plot.new()
      }
    }
  }
}

dev.off()

# Q3+Q4 only
png(filename=paste0(output.dir, "Survey indices - Q3+Q4 relative abundance by age and year ",min(yrs),"-",max(yrs),".png"), height=7, width=11, res=300, units="in")

mat <- matrix(1:9,nrow=9,ncol=length(yrs)) +t(matrix(seq(0,9*(length(yrs)-1),9),nrow=length(yrs),ncol=9))
layout(mat)
par(mar=c(0,0.5,1,0.5)+0.1, mgp=c(1.9, 0.8, 0))
for (yy in yrs){
  for (q. in qtrs[2]){
    for (a in 0:8){
      ally_tmp <- ally[ally$year %in% yy & ally$quarter %in% q. & ally$age %in% a,]
      tmp <- toplot$grid[[q.]]
      
      if(nrow(ally_tmp)>0){
        lab <- paste0(q.,"_",yy,":",a)
        
        #plot
        plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, 
             cex = map.cex, 
             axes = FALSE)
        box()
        title(lab, line = 0.2,cex.main=0.8)
        points(tmp$lon, y = tmp$lat, col = colors[as.numeric(ally_tmp$zFac)], 
               pch = 16, cex = map.cex)
        maps::map("worldHires", xlim = xlims, ylim = ylims, 
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
      }else{
        plot.new()
      }
    }
  }
}

dev.off()

# by cohort

png(filename=paste0(output.dir, "Survey indices - relative abundance by cohort ",
                    min(cohorts),"-",max(cohorts),".png"), height=7, width=15, res=300, units="in")
nn <- length(cohorts)
mat <- matrix(1:nn,nrow=nn,ncol=2*length(ages)+2) +t(matrix(seq(0,nn*(2*(length(ages))+2-1),nn),
                                                            nrow=2*length(ages)+2,ncol=nn))
layout(mat)
par(mar=c(0,0.5,1,0.5)+0.1, mgp=c(1.9, 0.8, 0))

for (a in 0:8){
  for (q. in qtrs){
    for (ch in cohorts){
      ally_tmp <- ally[ally$cohort %in% ch & ally$quarter %in% q. & ally$age %in% a,]
      tmp <- toplot$grid[[q.]]
      
      if(nrow(ally_tmp)>0){
        lab <- paste0(ch," ",q.,":",a)
        
        #plot
        plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, 
             cex = map.cex, 
             axes = FALSE)
        box()
        title(lab, line = 0.2,cex.main=0.8)
        points(tmp$lon, y = tmp$lat, col = colors[as.numeric(ally_tmp$zFac)], 
               pch = 16, cex = map.cex)
        maps::map("worldHires", xlim = xlims, ylim = ylims, 
                  fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
      }else{
        plot.new()
      }
    }
  }
}

dev.off()
