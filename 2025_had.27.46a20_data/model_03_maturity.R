#######################################################################
# Maturity ogive calculations for Northern Shelf haddock              
# COMBINED SEXES                                                      
# Northern Shelf region                                               
# Author: Aaron Brazier                                               
# Date created: 24/05/2021 12:15:18                                   
# Last edited: 21/03/2025                                   
# Version: 3                                                          
#----------------------------------------####

#start.tt <- now()

# load input data and settings -------------------------------------------

load("data/data_init.RData")
fyear <- ay

region <- "NShelf" # just do N Shelf
sx <- "F" # just do female
sx_name <- "Female"
dataG_filename <- "dataG_individual_sexes_no_sexratio.csv"
root_out <- "individual_sexes_no_sexratio"

#Read data and correct years
haddock <- read.table(paste0("data/Maturity/",dataG_filename), head=T, sep=',')
haddock <- haddock[haddock$year > 1990, ]

had.data <- haddock


# Calculate Ogive for NShelf wholly ---------------------------------------

#Create model to calculate ogive for individual sexes in North shelf area
modE2 <- glm(mat ~ age * as.factor(year) * sex, weight=WT, family=binomial("logit"), data=had.data) 
res.F <- a50Ffun(had.data)
#res.M <- a50Mfun(had.data) # not used

#Combine A50's into a single data frame and remove 1990 due to data errors
a50F <- data.frame(year=c(1991:fyear), res.F)
#a50M <- data.frame(year=c(1991:fyear), res.M) # not used

windows()
plot(a50F, ylim=c(0,3),type="l")
#lines(a50M,col="blue")

#Bootstrapping A50 of both sexes individually and then combined
#WARNING: Takes a long time to process.
set.seed(1995)
bootfit_F <- boot(data = had.data,
                  strata = had.data$year,
                  statistic = a50Ffun,
                  R = 999)

L <- length(c(1991:fyear))

cisF <- do.call(rbind, lapply(1:L, getCI, x=bootfit_F))
rezbootF <- data.frame(year=c(1991:fyear), A50f=bootfit_F$t0, cisF[,3:4])
write.taf(rezbootF, paste0("A50_",region,"_",root_out,"_Female.csv"), dir="model/")

# bootfit_M <- boot(data = had.data,
#                   strata = had.data$year,
#                   statistic = a50Mfun,
#                   R = 999)
# cisM <- do.call(rbind, lapply(1:L, getCI, x=bootfit_M))
# rezbootM <- data.frame(year=c(1991:fyear), A50m=bootfit_M$t0, cisM[,3:4])
# write.taf(rezbootM, paste0("A50_",region,"_",root_out,"_Male.csv"), dir="data/Maturity/")

print("A50 done")
#print(now()-start.tt)

#Produce ogives
ny <- (fyear - 1991) + 1
#Bootstrap
bootMF <- boot(data = had.data,
               strata = had.data$year,
               statistic = ogMF,
               R = 999)

L <- length(bootMF$t0)
rezbootFM <- data.frame(year=rep(c(1991:fyear),each=8), age=rep(c(1:8),ny), sex=rep(c("F","M"), each=8*ny), 
                        mat=round(bootMF$t0,2), lwr=rep(0,L), upr=rep(0,L))


rounded<-round(bootMF$t,4)
for(i in 1:L){
  if(length(levels(as.factor(rounded[,i]))) > 1) {
    a<-boot.ci(bootMF, type="basic", index=i)
    rezbootFM[i,5]<-round(a$basic[4],2)
    rezbootFM[i,6]<-round(a$basic[5],2)
  } else {
    rezbootFM[i,5]<-round(bootMF$t0[i],2)
    rezbootFM[i,6]<-round(bootMF$t0[i],2)
  }
}


ogiveMF <- rezbootFM
write.taf(ogiveMF, paste0("Mat_ogive_",region,"_",root_out,".csv"), dir="model/")

print("Raw ogive done")
#print(now()-start.tt)

# Smoothing
Mat0 <- ogiveMF 
Mat_sx <- Mat0[Mat0$sex == sx, ]
Mat_sx<-subset(Mat_sx, year<=fyear & year>1990)

Mat_sx<-Mat_sx[,c(1,2,4)]
raw_mat_sx <- reshape(Mat_sx,idvar=c("year"),timevar="age",direction="wide") 

write.taf(raw_mat_sx,paste0("raw_matogive_",region,"_",root_out,"_",sx_name,".csv"), dir="model/")

##With plus-group
library(gam)
#library(mgcv)
#detach("package:mgcv", unload=TRUE)
print("doing the smoothing")
Mat_sx$age[Mat_sx$age>6]<-6

for (i in 1:6) {                             
  M<-subset(Mat_sx, age==i)
  
  g<-gam::gam(mat~s(year,4),data=M,family=gaussian)
  s<-predict.Gam(g)
  s<-cbind(M,s)
  s<-subset(s,select=c(year,age,s))
  if (i==1) {
    bericht<-s
    aic<-(summary(g))$aic
  }else {
    bericht<-rbind(bericht,s)
    aic<-rbind(aic,(summary(g))$aic)}
}

matog_sx <- subset(bericht, select=c(year,age,s))
matog_sx <- reshape(matog_sx, idvar=c("year"), timevar="age", direction="wide")

matog_sx <- data.frame(matog_sx[,1], 0, matog_sx[,2:7])
matog_sx$s.7 <- matog_sx$s.6
matog_sx$s.8 <- matog_sx$s.6

matog_sx[matog_sx>1 & matog_sx<2] <- 1  #Values larger than 1 set to 1

colnames(matog_sx) <- c("Year", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7", "Age8")

matogfull <- matog_sx
matogfull$Age9 <- matogfull$Age6
matogfull$Age10 <- matogfull$Age6
matogfull$Age11 <- matogfull$Age6
matogfull$Age12 <- matogfull$Age6
matogfull$Age13 <- matogfull$Age6
matogfull$Age14 <- matogfull$Age6
matogfull$Age15 <- matogfull$Age6

write.taf(matogfull,paste0("Smoothed_mat_ogive_plusg15_",region,"_",root_out,"_",sx_name,".csv"), dir="model/",row.names=F)
write.taf(matog_sx,paste0("Smoothed_mat_ogive_plusg8_",region,"_",root_out,"_",sx_name,".csv"), dir="model/",row.names=F)

dat <- matogfull[, -1]
ex <- dat[1,]
dat <- rbind(ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,ex,dat)

maturityplus <- cbind(c(1978:fyear), dat)
colnames(maturityplus) <- c("Year", "age0", "age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15")

write.taf(maturityplus, paste0("Smoothed_mat_ogive_plusg15_",region,"_",root_out,"_",sx_name,"_full_timeseries.csv"), dir="model/",row.names=F)

print("smoothing done")
#print(now()-start.tt)


# save chosen maturity ogive
mat <- read.csv("model/Smoothed_mat_ogive_plusg8_NShelf_individual_sexes_no_sexratio_Female.csv")
#mat$Year <- mat[,1]
# extend back to 1965
tmp <- data.frame(matrix(NA,nrow=length(1965:1990),ncol=ncol(mat),dimnames = list(1:length(1965:1990),colnames(mat))))
tmp[1:nrow(tmp),] <- mat[1,]
tmp$Year <- 1965:1990
mat <- rbind(tmp,mat)
mat <- round(mat,3)
# save out file
write.csv(mat, "model/had.27.46a20 - Maturity ogive.csv", row.names=F)


#-------------------------------------------------------------------------##
# Retro analysis ####

#load dataG
if(1){
  start.tt <- now()
  region <- "NShelf" # just do N Shelf
  sx <- "F" # just do female
  sx_name <- "Female"
  dataG_filename <- "dataG_individual_sexes_no_sexratio.csv"
  root_out <- "individual_sexes_no_sexratio"
  
  #Read data and correct years
  haddock <- read.table(paste0("data/Maturity/",dataG_filename), head=T, sep=',')
  haddock <- haddock[haddock$year > 1990, ]
  
  print("start retros")
  #print(now())
  for (fyr in (ay-5):(ay-1)){
    fyear <- fyr
    # strip data from dataG - year by year
    ny <- (fyr - 1991) + 1
    had.data <- haddock[haddock$year <= fyr,]
    
    # redo ogive
    bootMF <- boot(data = had.data,
                   strata = had.data$year,
                   statistic = ogMF,
                   R = 999)
    
    L <- length(bootMF$t0)
    rezbootFM <- data.frame(year=rep(c(1991:fyear),each=8), age=rep(c(1:8),ny), 
                            sex=rep(c("F","M"), each=8*ny), 
                            mat=round(bootMF$t0,2), lwr=rep(0,L), upr=rep(0,L))
    
    rounded<-round(bootMF$t,4)
    for(i in 1:L){
      if(length(levels(as.factor(rounded[,i]))) > 1) {
        a<-boot.ci(bootMF, type="basic", index=i)
        rezbootFM[i,5]<-round(a$basic[4],2)
        rezbootFM[i,6]<-round(a$basic[5],2)
      } else {
        rezbootFM[i,5]<-round(bootMF$t0[i],2)
        rezbootFM[i,6]<-round(bootMF$t0[i],2)
      }
    }
    ogiveMF <- rezbootFM
    
    # redo smoothing
    Mat0 <- ogiveMF
    Mat_sx <- Mat0[Mat0$sex == sx, ]
    Mat_sx<-subset(Mat_sx, year<=fyear & year>1990)
    
    Mat_sx<-Mat_sx[,c(1,2,4)]
    
    ##With plus-group
    library(gam)
  #  library(mgcv)
  #  detach("package:mgcv", unload=TRUE)
    
    Mat_sx$age[Mat_sx$age>6]<-6
    
    for (i in 1:6) {                             
      M<-subset(Mat_sx, age==i)
      
      g<-gam::gam(mat~s(year,4),data=M,family=gaussian)
      s<-predict.Gam(g)
      s<-cbind(M,s)
      s<-subset(s,select=c(year,age,s))
      if (i==1) {
        bericht<-s
        aic<-(summary(g))$aic}
      else {
        bericht<-rbind(bericht,s)
        aic<-rbind(aic,(summary(g))$aic)}
    }
    
    matog_sx <- subset(bericht, select=c(year,age,s))
    matog_sx <- reshape(matog_sx, idvar=c("year"), timevar="age", direction="wide")
    
    matog_sx <- data.frame(matog_sx[,1], 0, matog_sx[,2:7])
    matog_sx$s.7 <- matog_sx$s.6
    matog_sx$s.8 <- matog_sx$s.6
    
    matog_sx[matog_sx>1 & matog_sx<2] <- 1  #Values larger than 1 set to 1
    
    colnames(matog_sx) <- c("Year", "Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", 
                            "Age7", "Age8")
    
    # save results
    write.taf(matog_sx,
              paste0("Retro_Smoothed_mat_ogive_plusg8_",region,"_",root_out,"_",sx_name,"_",fyr,".csv"),
              dir="model/")
    
    print(paste0("retro done: ",fyear))
    #print(now()-start.tt)
  }
}

