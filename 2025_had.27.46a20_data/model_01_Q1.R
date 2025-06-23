
##  Combined index for Northern Shelf haddock - Q1 
##  Apply model


# Set up
rm(list=ls())

# Settings
load("data/data_init.RData")
set.seed(1988)

# load data -------------------------------------------------####

load(paste0("data/dQ1withALK_",ay,".RData"))


# Model -----------------------------------------------------####

grid <- getGrid(dQ1, nLon=40)
tryCatch.W.E <- DATRAS:::tryCatch.W.E 
SI.alt <- getSurveyIdxStratMean(dQ1, agesQ1)

gridd <- subset(dQ1[[2]], haul.id %in% grid[[3]])

##### post review -  alternative model 3 post review - add 5 mins to haul duration
modelZ3 <- rep("Year + Gear + s(Ship, bs='re', by=dum) + s(lon,lat,bs='ds',k=80,m=c(1,0.5)) + s(lon,lat,bs='ds',k=7,m=c(1,0.5),by=Year,id=1) + 
            s(Depth,bs='ds',k=6) + s(TimeShotHour,bs='cc',k=6)+ s(timeOfYear, bs='ds', k=6) + offset(log(HaulDur+5))",length(agesQ1))

modelP3 <- rep("Year + Gear + s(Ship, bs='re', by=dum) + s(lon,lat,bs='ds',k=120,m=c(1,0.5)) + s(lon,lat,bs='ds',m=c(1,0.5),k=9,by=Year,id=1) + 
            + s(Depth, bs='ds', k=6) + s(TimeShotHour, bs='cc', k=6) + s(timeOfYear, bs='ds', k=6) + offset(log(HaulDur+5))",length(agesQ1))

SI <- getSurveyIdx(dQ1, ages=agesQ1, myids=NULL,predD=gridd, cutOff=0.14, fam="LogNormal", mc.cores=mc.cores, 
                   modelZ=modelZ3, modelP=modelP3,control=list(trace=TRUE,maxit=10))


save(list=c("SI","SI.alt","grid","gridd"), file="model/SImodel_Q1.RData")


# Retro analysis -----------------------------------------------------------------------####
if(runRetro){
  
  load(paste0("data/dQ1withALK_",ay,".RData"))
  
  # choose model alt 3
  load(paste0("model/SImodel_Q1.RData"))
  
  modelZ3 <- rep("Year + Gear + s(Ship, bs='re', by=dum) + s(lon,lat,bs='ds',k=80,m=c(1,0.5)) + s(lon,lat,bs='ds',k=7,m=c(1,0.5),by=Year,id=1) + 
            s(Depth,bs='ds',k=6) + s(TimeShotHour,bs='cc',k=6)+ s(timeOfYear, bs='ds', k=6) + offset(log(HaulDur+5))",length(agesQ1))
  
  modelP3 <- rep("Year + Gear + s(Ship, bs='re', by=dum) + s(lon,lat,bs='ds',k=120,m=c(1,0.5)) + s(lon,lat,bs='ds',m=c(1,0.5),k=9,by=Year,id=1) + 
            + s(Depth, bs='ds', k=6) + s(TimeShotHour, bs='cc', k=6) + s(timeOfYear, bs='ds', k=6) + offset(log(HaulDur+5))",length(agesQ1))
  
  ##### Manual version
  
  years.inc <- yearsQ1
  last.yrs <- rev((ay-5):ay)
  
  ret <- list()
  for (i in 1:length(last.yrs)) {
    print(last.yrs[i])
    d.tmp <- subset(dQ1, Year %in% years.inc[1]:last.yrs[i])
    gr.tmp <- getGrid(dQ1, nLon=40)
    gridd.tmp <- subset(dQ1[[2]], haul.id %in% grid[[3]])
    
    index.tmp <- getSurveyIdx(d.tmp, ages=agesQ1, myids=NULL,predD=gridd.tmp, cutOff=0.14, fam="LogNormal", mc.cores=mc.cores,
                              modelZ=modelZ3, modelP=modelP3,nBoot=1000,control=list(trace=FALSE,maxit=10))
    
    ret[[i]] <- index.tmp$idx
  #  print(index.tmp$idx)
    rm(index.tmp)
  }
  
  save(ret, file="model/SI_retro_model_Q1.RData")
  
}
