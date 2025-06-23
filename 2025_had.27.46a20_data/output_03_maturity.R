## Extract results of interest, write TAF output tables

## Before:
## After:

load("data/data_init.RData")

# load data ---------------------------------------------------

dataG <- read.taf("data/Maturity/dataG_individual_sexes_no_sexratio.csv")
mo_raw <- read.csv("model/raw_matogive_NShelf_individual_sexes_no_sexratio_Female.csv")
mo_smo <- read.csv("model/had.27.46a20 - Maturity ogive.csv")

# Maturity ####

# plot number of samples per year - dataG

tmp <- tapply(dataG$nb.x,list(dataG$year,dataG$sex),sum)

png(paste0(output.dir,"Maturity ogive - number of samples.png"),width = 10, height = 7, units = "in", res = 600)
plot(as.numeric(rownames(tmp)),tmp[,"F"], xlab="",ylab="Number of samples",type="o",pch=16)
dev.off()


# plot raw vs smoothed ogives

mo_raw <- pivot_longer(mo_raw,cols=c(paste0("mat.",1:8)),names_to="age",values_to="mat")
mo_raw$age <- gsub("mat.","",mo_raw$age)
mo_raw$label <- "raw"

mo_smo <- pivot_longer(mo_smo,cols=c(paste0("Age",0:8)),names_to="age",values_to="mat")
mo_smo$age <- gsub("Age","",mo_smo$age)
colnames(mo_smo) <- gsub("Year","year",colnames(mo_smo))
mo_smo <- filter(mo_smo,age>0)
mo_smo$label <- "smoothed"

dat <- bind_rows(mo_raw,mo_smo)

png(paste0(output.dir,"Maturity ogive - raw vs smoothed.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=year, y=mat,colour=age,linetype=label)) + 
  geom_line() + theme_bw()+ labs(y="Proportion mature",colour="Age",x="",linetype="") +
  scale_colour_manual(values=col.pal)+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=8),
        legend.text=element_text(size=9)) 
print(p1)
dev.off()


# maturity  - diff from long term mean by age. to show periods of DD

dat <- mo_raw

# get cohort
dat <- dat %>% rowwise() %>% mutate(cohort=year-as.numeric(age) )

# diff in mean weight by age
tmp <- dat %>% group_by(age) %>% summarise(mMat=mean(mat))
dat <- left_join(dat,tmp)
dat <- dat %>% mutate(Mdiff=mat-mMat)

# age labels
dat <- filter(dat,age<5)
dat$age <- as.factor(dat$age)

# iceland plot
toplot <- dat
p1 <- ggplot(toplot,aes(x=year,y=Mdiff,colour=cohort,fill=cohort))+geom_col()+theme_bw()+
  facet_wrap(~age,scales="free_y",ncol=1,strip.position="right") + 
  labs(x="",y="Proportion mature difference")
png(file=paste0(output.dir,"Maturity ogive - mean difference over time by age.png"),height=11,width=7,res=200,units="in")
print(p1)
dev.off()


# plot retro results
# create table
ret <- matrix(NA,nrow=length(1991:ay),ncol=6,dimnames=list(1991:ay,(ay-5):ay))

# read in files
for (fyr in (ay-5):(ay)){
  if(fyr ==ay){
    tmp <- read.taf(paste0("model/Smoothed_mat_ogive_plusg8_NShelf_individual_sexes_no_sexratio_Female.csv"))
  }else{
    tmp <- read.taf(paste0("model/Retro_Smoothed_mat_ogive_plusg8_NShelf_individual_sexes_no_sexratio_Female_",fyr,".csv"))
  }
  
  for (ag in 1:5){
    if(fyr == ay-5){
      assign("dat",ret)
      dat[as.character(1991:fyr),as.character(fyr)] <- tmp[,paste0("Age",as.character(ag))]
      assign(paste0("ret",ag),dat)
    }else{
      assign("dat",get(paste0("ret",ag)))
      dat[as.character(1991:fyr),as.character(fyr)] <- tmp[,paste0("Age",as.character(ag))]
      assign(paste0("ret",ag),dat)
    }
  }
}

# calc mohn's rho
rhos <- numeric(length(1:5))
for (ag in 1:5) {
  assign("dta",get(paste0("ret",ag)))
  windows()
  rhos[ag] <- icesAdvice::mohn(dta,peels=5)
  
  # plot
  ylims <- range(dta,na.rm=T)
  ylims[1] <- ylims[1]*0.9
  ylims[2] <- ylims[2]*1.1
  icesAdvice::mohn(dta, peels = 5, details = FALSE, plot = TRUE,ylim=ylims)
  title(main=paste0("Age ",ag,": Mohn's rho = ",round(rhos[ag],3)))
  savePlot(filename = paste0(output.dir,"Maturity ogive -  Mohn's rho - age ",ag,".png"),type = "png")
}
graphics.off()
