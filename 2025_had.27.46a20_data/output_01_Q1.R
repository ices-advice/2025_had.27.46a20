
graphics.off()

# load data and model outputs ---------------------------------------------------####

load("data/data_init.RData")

load(paste0("data/dQ1withALK_",ay,".RData"))

load(paste0("model/SImodel_Q1.RData"))

# Export results --------------------------------------------------------------####

sink(paste0(output.dir,"Q1internal_cons.txt"))
internalCons(SI$idx)
sink()

mean(internalCons(SI$idx))

# Final index (numbers per h)
ynum <- as.numeric(as.character(dQ1$Year))

exportSI(2*SI$idx[, ]/length(grid[[3]]), agesQ1[], min(ynum):max(ynum), toy=mean(dQ1[[2]]$timeOfYear, na.rm=TRUE), 
         file=paste(output.dir,"survey-haddock-Q1-", min(agesQ1), "-", max(agesQ1), "plus.dat", sep=""), 
         nam=paste("Haddock on Northern Shelf; Q1 index value; Last age is plus group, calculated", Sys.time()))

exportSI(2*SI$lo[, ]/length(grid[[3]]), agesQ1[], min(ynum):max(ynum), toy=mean(dQ1[[2]]$timeOfYear, na.rm=TRUE), 
         file=paste(output.dir,"survey-haddock-Q1-", min(agesQ1), "-", max(agesQ1), "plus_lo.dat", sep=""),
         nam=paste("Haddock on Northern Shelf; Q1 index lower CI; Last age is plus group, calculated", Sys.time()))

exportSI(2*SI$up[, ]/length(grid[[3]]), agesQ1[], min(ynum):max(ynum), toy=mean(dQ1[[2]]$timeOfYear, na.rm=TRUE), 
         file=paste(output.dir,"survey-haddock-Q1-", min(agesQ1), "-", max(agesQ1), "plus_hi.dat", sep=""),
         nam=paste("Haddock  on Northern Shelf; Q1 index upper CI; Last age is plus group, calculated", Sys.time()))


# Plots - results --------------------------------------------------------------####


# plot indices
png(filename=paste0(output.dir,"Survey indices - Q1 compare to SMM.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots_withShadedCI(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], select=c("index"),
                            par=list(mfrow=c(3,3), mar=c(3.1,3.1,2.1,1), mgp=c(1.9, 0.8, 0)), plotByAge=FALSE)
dev.off()

# Map abundance at age
png(filename=paste0(output.dir,"Survey indices - Q1 relative abundance - ",ay,".png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=grid[[3]], select=c("map"), year=ay, map.cex=1.5,
               par=list(mfrow=c(3,3), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)),plotByAge=FALSE, 
               colors=rev(heat.colors(5)), legend=FALSE)
dev.off()

# Plots - results - all years -----------------------------------------------------####

if(0){
  windows()
  pdf(file = paste0(output.dir,"Survey indices - Q1 CV maps.pdf"),paper="a4r")
  
  surveyIdxPlots(SI, dQ1, myids=NULL, predD=gridd, select="CVmap", year=yearsQ1, colors=cm.colors(6) ,plotByAge=FALSE,
                 par=list(mfrow=n2mfrow(nlevels(dQ1$Year)), mar=c(0,0,2,0), cex=0.6),
                 legend=TRUE, legend.signif=2, map.cex=1, cutp=c(0,0.2,0.4,0.6,1,2,Inf))
  dev.off()
  
  yy_select <-(ay-1):ay
  pdf(file = paste0(output.dir,"Survey indices - Q1 Relative abundance maps.pdf"),paper="a4r",width=11, height=7)
 
   for (yy in yy_select){

    surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("map"), year=yy,map.cex=1.5,
                  par=list(mfrow=c(length(yy_select),length(agesQ1)), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0),new=T), 
                   plotByAge=F,plotByYear=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  }
  dev.off()
  
  
  pdf(file = paste0(output.dir,"Survey indices - Q1 Absolute abundance maps.pdf"),paper="a4r",width=11, height=7)
  for (yy in yy_select){
    surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("absolutemap"), year=yy,map.cex=1.5,
                   par=list(mfrow=c(3,3), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), 
                   plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  }
  dev.off()
  
  pdf(file = paste0(output.dir,"Survey indices - Q1 Spatial residuals.pdf"),paper="a4r",width=11, height=7)
  for (yy in yearsQ1){
    surveyIndex::surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=yy,
                                par=list(mfrow=c(3,3), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), map.cex=0.6,
                                plotByAge=FALSE, colors=rev(heat.colors(5)), legend=FALSE)
  }
  dev.off()
  
}


# check 2025 - check

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - 2025 absolute map.png"), height=3, width=25, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("absolutemap"), year=ay,map.cex=1.5,
               par=list(mfrow=c(1,9), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), 
               plotByAge=FALSE, colors=rev(heat.colors(5)), legend=TRUE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - 2024 absolute map.png"), height=3, width=25, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("absolutemap"), year=ay-1,map.cex=1.5,
               par=list(mfrow=c(1,9), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), 
               plotByAge=FALSE, colors=rev(heat.colors(5)), legend=TRUE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - 2025 alt spatial resids.png"), height=3, width=25, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=ay,map.cex=0.6,
               par=list(mfrow=c(1,9), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), 
               plotByAge=FALSE, colors=rev(heat.colors(5)), legend=TRUE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - 2024 alt spatial resids.png"), height=3, width=25, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=ay-1,map.cex=0.6,
               par=list(mfrow=c(1,9), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), 
               plotByAge=FALSE, colors=rev(heat.colors(5)), legend=TRUE)
dev.off()
# Plots - diagnostics --------------------------------------------------------------####


png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - stations by Survey.png"), height=7, width=9, res=300, units="in")
par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Survey))
legend("bottomright", levels(dQ1[[2]]$Survey), col=1:length(levels(dQ1[[2]]$Survey)), pch=16, cex=0.9,bg="white")
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - stations by Gear.png"), height=7, width=9, res=300, units="in")
par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Gear))
legend("bottomright", levels(dQ1[[2]]$Gear), col=1:length(levels(dQ1[[2]]$Gear)), pch=16,bg="white")
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - stations by Country.png"), height=7, width=9, res=300, units="in")
par(mar=c(3.8,3.8,0.5,1),  mgp=c(2.2, 0.8, 0))
mybubblePlot(dQ1, xlim=c(-10, 15), ylim=c(51, 62), col.zero=grey(0.7), scale=0.0075, col.cty=as.numeric(dQ1[[2]]$Country))
legend("bottomright", levels(dQ1[[2]]$Country), col=1:length(levels(dQ1[[2]]$Country)), pch=16,bg="white")
dev.off()


png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - ship effect.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt,myids=NULL,predD=gridd, select=c("1"), scheme=1,
               par=list(mfrow=c(3,3), mar=c(3.1,3.1,1.5,1), mgp=c(1.9, 0.8, 0)), plotByAge=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - spatial effect.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("2"),scheme=2, residuals=F, rug=T,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)), plotByAge=FALSE)
dev.off()


n <- length(yearsQ1)+2


png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - annual spatial effect ",ay,".png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=as.character(n), scheme=2, residuals=F, rug=T,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()


png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - depth effect.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=as.character(n+1), scheme=1, residuals=F, rug=T,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - time of day effect.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=as.character(n+2), scheme=1, residuals=F, rug=T,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - time of year effect.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=as.character(n+3), scheme=1, residuals=F, rug=T,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()


png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - normalised residuals.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("residuals"),
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - spatial residuals - ",ay,".png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=ay, map.cex=0.6,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE,colors=rev(heat.colors(5)), legend=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - spatial residuals - ",ay-1,".png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=ay-1, map.cex=0.6,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE,colors=rev(heat.colors(5)), legend=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - spatial residuals - ",ay-2,".png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("spatialResiduals"), year=ay-2, map.cex=0.6,
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE,colors=rev(heat.colors(5)), legend=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - annual residuals.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("resVsYear"),
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()

png(filename=paste0(output.dir,"Survey indices - Q1 diagnostics - residuals vs fitted values.png"), height=7, width=11, res=300, units="in")
surveyIdxPlots(SI, dQ1, alt.idx=SI.alt, myids=NULL,predD=gridd, select=c("fitVsRes"),
               par=list(mfrow=c(3,3), mar=c(3.1,3,1.5,1), mgp=c(1.9, 0.8, 0)),  plotByAge=FALSE)
dev.off()



# Plot to compare to initial results

new <- as.data.frame(2*SI$idx[, ]/length(grid[[3]]))
new$Year <- rownames(new)
load("model/SImodel_Q1 - initial.RData")
old <- as.data.frame(2*SI$idx[, ]/length(grid[[3]]))
old$Year <- rownames(old)

library(tidyverse)
new <- pivot_longer(new,cols=as.character(1:8),names_to="Age",values_to="index")
new$label <- "final"
old <- pivot_longer(old,cols=as.character(1:8),names_to="Age",values_to="index")
old$label <- "initial"

dat <- bind_rows(new,old)

png(filename=paste0(output.dir,"Survey indices - Q1 compare initial and final_new_cutoff.png"), height=7, width=11, res=300, units="in")

p1 <- ggplot(dat,aes(x=as.numeric(Year),y=index,colour=label,linetype=label))+facet_wrap(~Age,scales="free_y")+
  geom_line()+
  theme_bw()+labs(x="",colour="",linetype="")
print(p1)
dev.off()


# Plot - Q1 -------------------------------------------------------------------
load(paste0("model/SImodel_Q1.RData"))
library(tidyverse)

# Get  Q1 index and CI
tmp1 <- as.data.frame(2*SI$idx[,]/length(grid[[3]]))
tmp1$year <- yearsQ1
tmp1 <- pivot_longer(tmp1,cols=as.character(agesQ1),names_to="age",values_to="index")

tmp2 <- as.data.frame(2*SI$lo[,]/length(grid[[3]]))
tmp2$year <- yearsQ1
tmp2 <- pivot_longer(tmp2,cols=as.character(agesQ1),names_to="age",values_to="low")

tmp3 <- as.data.frame(2*SI$up[,]/length(grid[[3]]))
tmp3$year <- yearsQ1
tmp3 <- pivot_longer(tmp3,cols=as.character(agesQ1),names_to="age",values_to="high")

dat1 <- left_join(full_join(tmp1,tmp2),tmp3)
dat1$label <- "NS-WC Q1"

# combine and plot
dat <- dat1

png(filename=paste0(output.dir,"Survey indices - Q1 with CI.png"), height=7, width=11, res=300, units="in")
p1 <- ggplot(dat,aes(x=year,y=index,ymin=low,ymax=high))+facet_wrap(~age,scales="free_y")+
  geom_line()+geom_ribbon(alpha=0.3,linetype="blank")+theme_bw()+labs(x="",colour="",fill="")
print(p1)
dev.off()




# Plots - retro --------------------------------------------------------------####

# retro

if(runRetro){
  load("model/SImodel_Q1.RData")
  load("model/SI_retro_model_Q1.RData")
  
  npeels <- 5
  n.non.peeled <- 3
  years.inc <- 1983:ay
  last.yr <- rev(tail(years.inc, npeels+1)) 
  ret.peels <- ret[1:(1+npeels)]
  
  
  for (i in 1:length(last.yr)) ret[[i]]=2*ret[[i]]/length(grid[[3]])
  colfunc <- colorRampPalette(c("red2", "blue"))
  
  rhos <- numeric(length(agesQ1))
  for (a in 1:length(agesQ1)) {
    dta <- as.data.frame(matrix(nrow=npeels+1+n.non.peeled, ncol=npeels+1, dimnames=list(as.character(tail(years.inc, npeels+1+n.non.peeled)), c("base", -(1:npeels)) ))) 
    for (i in 1:(npeels+1)) 
      dta[1:(npeels-i+2+n.non.peeled), i] <- tail(ret.peels[[i]][, a], npeels-i+2+n.non.peeled)
    rhos[a] <- icesRound(icesAdvice::mohn(dta))
  }
  
  
  png(filename=paste0(output.dir,"Survey indices - Q1 Retros.png"), height=7, width=9, bg="white", pointsize=12, res=300, units="in")
  par(mfrow=c(3, 3), mar=c(3.1, 3.1, 1, 1), mgp=c(1.9, 0.8, 0))
  for (a in 1:length(agesQ1)) {
    maxy <- NULL
    for (i in 1:length(ret.peels)) maxy <- c(maxy, ret.peels[[i]][, a])
    maxy <- max(maxy)
    plot(NA, NA, type="l", lwd=1, xlab="Index", ylab="Year", main=paste("Age group", colnames(dQ1$Nage)[a]), xlim=range(years.inc), ylim=c(0, maxy))
    for (i in length(ret.peels):1)
      lines(as.numeric(rownames(ret.peels[[i]])), ret.peels[[i]][, a], lty=1, lwd=1, col=adjustcolor(colfunc(length(ret.peels)))[i])
    legend("topright", legend=paste0("rho=", rhos[a]), bty="n", inset=0.0)
  }
  dev.off()
  
}
