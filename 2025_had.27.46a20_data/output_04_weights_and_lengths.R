## Extract results of interest, write TAF output tables

## Before:
## After:


load("data/data_init.RData")

# load data ---------------------------------------------------

wt_comb <- read.csv("data/Weights and lengths/mean_weights_combinedRegions.csv")
len_comb <- read.csv("data/Weights and lengths/mean_length_combinedRegion.csv")
freq_comb <- read.csv("data/Weights and lengths/mean_length_freq_combinedRegion.csv")


# mean weights -----------------------------------------------

dat <- wt_comb
dat$year <- as.numeric(dat$year)
dat$age <- as.numeric(dat$age)

#calculate variables to plot

# get cohort
dat <- dat %>% rowwise() %>% mutate(cohort=year-age) 

# diff in mean weight by age
tmp <- dat %>% group_by(age) %>% summarise(mWt=mean(weight))
dat <- left_join(dat,tmp)
dat <- dat %>% mutate(Wdiff=weight-mWt)

# age labels
dat$age <- as.factor(dat$age)

# weight over time by age
toplot <- dat
p1 <- ggplot(toplot,aes(x=year, y=weight,colour=age))+geom_point()+geom_smooth()+theme_bw()+
  scale_colour_manual(values=col.pal)+facet_wrap(~age,scales="free_y")+labs(x="",colour="",y="mean weight (g)")
png(file=paste0(output.dir,"Mean weights - mean weight over time by age.png"),height=7,width=11,res=200,units="in")
print(p1)
dev.off()

# time series of cohorts
toplot <- filter(dat,as.numeric(age)<7)
p1 <- ggplot(toplot,aes(x=year, y=weight, group=cohort,colour=cohort))+geom_point()+geom_line()+theme_bw()+
  labs(x="",colour="cohort",y="mean weight (g)")
png(file=paste0(output.dir,"Mean weights - mean weight over time by cohort.png"),height=7,width=11,res=200,units="in")
print(p1)
dev.off()

# iceland plot
toplot <- dat
p1 <- ggplot(toplot,aes(x=year,y=Wdiff,colour=cohort,fill=cohort))+geom_col()+theme_bw()+
  #  scale_colour_manual(values=col.pal)+scale_fill_manual(values=col.pal)+
  facet_wrap(~age,scales="free_y",ncol=1,strip.position="right") + 
  labs(x="",y="Weight difference (g)")
png(file=paste0(output.dir,"Mean weights - mean weight difference over time by age.png"),height=11,width=7,res=200,units="in")
print(p1)
dev.off()


# mean length ---------------------------------------------------------

dat <- len_comb
dat$year <- as.numeric(dat$year)
dat$age <- as.numeric(dat$age)

#calculate variables to plot

# get cohort
dat <- dat %>% rowwise() %>% mutate(cohort=year-age) 

# diff in mean length by age
tmp <- dat %>% group_by(age) %>% summarise(mLn=mean(length))
dat <- left_join(dat,tmp)
dat <- dat %>% mutate(Ldiff=length-mLn)

# age labels
dat$age <- as.factor(dat$age)

# length over time by age
toplot <- dat
p1 <- ggplot(toplot,aes(x=year, y=length,colour=age))+geom_point()+geom_smooth()+theme_bw()+
  scale_colour_manual(values=col.pal)+facet_wrap(~age,scales="free_y")+labs(x="",colour="",y="mean length (cm)")
png(file=paste0(output.dir,"Mean length - mean length over time by age.png"),height=7,width=11,res=200,units="in")
print(p1)
dev.off()

# time series of cohorts
toplot <- filter(dat,as.numeric(age)<7)
p1 <- ggplot(toplot,aes(x=year, y=length, group=cohort,colour=cohort))+geom_point()+geom_line()+theme_bw()+
  labs(x="",colour="cohort",y="mean length (cm)")
png(file=paste0(output.dir,"Mean length - mean length over time by cohort.png"),height=7,width=11,res=200,units="in")
print(p1)
dev.off()

# iceland plot
toplot <- dat
p1 <- ggplot(toplot,aes(x=year,y=Ldiff,colour=cohort,fill=cohort))+geom_col()+theme_bw()+
  #  scale_colour_manual(values=col.pal)+scale_fill_manual(values=col.pal)+
  facet_wrap(~age,scales="free_y",ncol=1,strip.position="right") + 
  labs(x="",y="Length difference (cm)")
png(file=paste0(output.dir,"Mean length - mean length difference over time by age.png"),height=11,width=7,res=200,units="in")
print(p1)
dev.off()


# length freq by age and year

# plot weight/length diff from long term mean against SSB/total stock biomass, or in numbers, when cohort is age 0, 1, 2, 3 etc