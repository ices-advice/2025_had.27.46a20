
##  Combined index for Nothern Shelf haddock - Q3Q4 
##  Apply model


# Set up
rm(list=ls())

# Settings
load("data/data_init.RData")
set.seed(1988)

# load data -------------------------------------------------####

load(paste0("data/dQ3Q4withALK_",ay-1,".RData"))

# Model -----------------------------------------------------####

grid = getGrid(dQ3Q4, nLon=40)
tryCatch.W.E <- DATRAS:::tryCatch.W.E 
SI.alt = getSurveyIdxStratMean(dQ3Q4, agesQ3Q4+1)

gridd <- subset(dQ3Q4[[2]], haul.id %in% grid[[3]])


#####  alternative model 4 - use log(Depth), post review - add 5 mins to haul duration
modelZ3 <- rep("s(lon, lat, bs='ds', k=80,m=c(1,0.5)) + s(lon,lat,bs='ds',k=7,m=c(1,0.5),by=Year,id=1) +
Year + s(log(Depth), bs='ts', k=6) + s(Ship, bs='re', by=dum) + Gear + s(timeOfYear, bs='ts', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + s(TimeShotHour, bs='cc', k=6, by=Quarter) + offset(log(HaulDur+5))", length(agesQ3Q4))

modelP3 <- rep("s(lon, lat, bs='ds', k=120,m=c(1,0.5)) + s(lon,lat,bs='ds',m=c(1,0.5),k=9,by=Year,id=1) + 
Year + s(log(Depth), bs='ts', k=6) + s(Ship, bs='re', by=dum) + Gear + s(timeOfYear, bs='ts', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + s(TimeShotHour, bs='cc', k=6, by=Quarter) + offset(log(HaulDur+5))", length(agesQ3Q4))

SI <- getSurveyIdx(dQ3Q4, ages=agesQ3Q4, myids=NULL,predD=gridd, cutOff=0.1, fam="LogNormal", mc.cores=mc.cores, 
                   modelZ=modelZ3, modelP=modelP3,control=list(trace=TRUE,maxit=10))

save(list=c("SI","SI.alt","grid","gridd"), file="model/SImodel_Q3Q4.RData")

# Retro analysis -----------------------------------------------------------------------####

if(runRetro){
  
  load(paste0("data/dQ3Q4withALK_",ay-1,".RData"))
  
  
  #####  alternative model 4 - use log(Depth)
  modelZ3 <- rep("s(lon, lat, bs='ds', k=80,m=c(1,0.5)) + s(lon,lat,bs='ds',k=7,m=c(1,0.5),by=Year,id=1) +
Year + s(log(Depth), bs='ts', k=6) + s(Ship, bs='re', by=dum) + Gear + s(timeOfYear, bs='ts', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + s(TimeShotHour, bs='cc', k=6, by=Quarter) + offset(log(HaulDur+5))", length(agesQ3Q4))
  
  modelP3 <- rep("s(lon, lat, bs='ds', k=120,m=c(1,0.5)) + s(lon,lat,bs='ds',m=c(1,0.5),k=9,by=Year,id=1) + 
Year + s(log(Depth), bs='ts', k=6) + s(Ship, bs='re', by=dum) + Gear + s(timeOfYear, bs='ts', k=6) + 
  s(TimeShotHour, bs='cc', k=6) + s(TimeShotHour, bs='cc', k=6, by=Quarter) + offset(log(HaulDur+5))", length(agesQ3Q4))  
  
  ##### Manual version
  
  years.inc <- yearsQ3Q4
  last.yrs <- rev((ay-6):(ay-1))
  
  ret <- list()
  for (i in 1:length(last.yrs)) {
    print(last.yrs[i])
    d.tmp <- subset(dQ3Q4, Year %in% years.inc[1]:last.yrs[i])
    gr.tmp <- getGrid(dQ3Q4, nLon=40)
    gridd.tmp <- subset(dQ3Q4[[2]], haul.id %in% grid[[3]])
    
    index.tmp <- getSurveyIdx(d.tmp, ages=agesQ3Q4, myids=NULL,predD=gridd.tmp, cutOff=0.1, fam="LogNormal", mc.cores=mc.cores,
                              modelZ=modelZ3, modelP=modelP3,nBoot=1000,control=list(trace=FALSE,maxit=10))
    
    ret[[i]] <- index.tmp$idx
 #   print(index.tmp$idx)
    rm(index.tmp)
  }
  
  save(ret, file="model/SI_retro_model_Q3Q4.RData")
  
}


