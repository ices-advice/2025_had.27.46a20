######################################################
# Haddock - pre-process survey data                  
# Author: Aaron Brazier                              
# Date created: 22/10/2021 15:07:09                  
# Last edited: 21/03/2025                            
# Version: 3                                         
######################################################    

#Set workspace parameters
load("data/data_init.RData")
fyear <- ay


# Step 1. Compile maturity data ------------------------------------------

# Load in data from last year
load(paste0("boot/data/Compiled exchange data haddock Q1 - WGNSSK ",ay-1,".RData"))

# Load ay data (previously saved when downloaded for the indices)
load(paste0("data/NS exchange data haddock Q1 ",ay,".RData"))
load(paste0("data/WC exchange data haddock Q1 ",ay,".RData"))

# Sort column names
NS.IBTS[[1]] <- NS.IBTS[[1]][,colnames(NS.IBTS[[1]])[!colnames(NS.IBTS[[1]]) %in% "LiverWeight"]]
NS.IBTS[[2]] <- NS.IBTS[[2]][,colnames(NS.IBTS[[2]])[!colnames(NS.IBTS[[2]]) %in% "SurveyIndexArea"]]
SCOWCGFS[[1]] <- SCOWCGFS[[1]][,colnames(SCOWCGFS[[1]])[!colnames(SCOWCGFS[[1]]) %in% "LiverWeight"]]
SCOWCGFS[[2]] <- SCOWCGFS[[2]][,colnames(SCOWCGFS[[2]])[!colnames(SCOWCGFS[[2]]) %in% "SurveyIndexArea"]]

NS.IBTS[[1]] <- cbind(NS.IBTS[[1]],data.frame(ScientificName_WoRMS = "Melanogrammus aeglefinus"))
SCOWCGFS[[1]]<- cbind(SCOWCGFS[[1]],data.frame(ScientificName_WoRMS = "Melanogrammus aeglefinus"))
NS.IBTS[[3]] <- cbind(NS.IBTS[[3]],data.frame(ScientificName_WoRMS = "Melanogrammus aeglefinus"))
SCOWCGFS[[3]]<- cbind(SCOWCGFS[[3]],data.frame(ScientificName_WoRMS = "Melanogrammus aeglefinus"))

#Combine individual survey/data files into single data frame
haddock[[1]] <- do.call("rbind", list(haddock[[1]],NS.IBTS[[1]],SCOWCGFS[[1]]))
haddock[[2]] <- do.call("rbind", list(haddock[[2]],NS.IBTS[[2]],SCOWCGFS[[2]]))
haddock[[3]] <- do.call("rbind", list(haddock[[3]],NS.IBTS[[3]],SCOWCGFS[[3]]))

#Confirm all surveys are present in the combined data frame
unique(haddock[[1]]$Survey)
unique(haddock[[2]]$Survey)
unique(haddock[[3]]$Survey)

#Step 2. Data manipulation ------------------------------------------------

#Subset to Q1 haddock data with only valid hauls and remove missing weights
haddock <- subset(haddock, Species==paste(genus,bfamily), Quarter==1, HaulVal=="V")

#Create an age 8+ group - use values found using: unique(haddock[["CA"]]$Age)
haddock[["CA"]]$Age[haddock[["CA"]]$Age >= 8] <- 8

#Add raw number of observed length group to HH data and calculate CPUE
haddock = addSpectrum(haddock, by=1)

#Add ages 1:8. please wait....
haddock = addNage(haddock, 1:8)

#Add weight by haul
haddock = addWeightByHaul(haddock)

#Standardise values for an effort of 60 minutes
haddock[["HH"]]$N       <- haddock[["HH"]]$N / haddock[["HH"]]$HaulDur * 60
haddock[["HH"]]$Nage    <- haddock[["HH"]]$Nage / haddock[["HH"]]$HaulDur * 60
haddock[["HH"]]$HaulWgt <- haddock[["HH"]]$HaulWgt / haddock[["HH"]]$HaulDur * 60


################################################################################
#Keep North Sea, West of Scotland, and Kattegat (Northern Shelf regions)

# set ICES rectangle lists to use later

#North Sea
NSea <- c("31F0","31F1","31F2","31F3",
          "32F0","32F1","32F2","32F3",
          "33F1","33F2","33F3","33F4",
          "34F0","34F1","34F2","34F3","34F4",
          "35F0","35F1","35F2","35F3","35F4","35F5","35F6",
          "36E9","36F0","36F1","36F2","36F3","36F4","36F5","36F6","36F7","36F8",
          "37E9","37F0","37F1","37F2","37F3","37F4","37F5","37F6","37F7","37F8",
          "38E8","38E9","38F0","38F1","38F2","38F3","38F4","38F5","38F6","38F7","38F8",
          "39E8","39E9","39F0","39F1","39F2","39F3","39F4","39F5","39F6","39F7","39F8",
          "40E6","40E7","40E8","40E9","40F0","40F1","40F2","40F3","40F4","40F5","40F6","40F7","40F8",
          "41E6","41E7","41E8","41E9","41F0","41F1","41F2","41F3","41F4","41F5","41F6","41F7","41F8",
          "42E7","42E8","42E9","42F0","42F1","42F2","42F3","42F4","42F5","42F6","42F7","42F8",
          "43E7","43E8","43E9","43F0","43F1","43F2","43F3","43F4","43F5","43F6","43F7",
          "44E6","44E7","44E8","44E9","44F0","44F1","44F2","44F3","44F4","44F5",
          "45E6","45E7","45E8","45E9","45F0","45F1","45F2","45F3","45F4",
          "46E6","46E7","46E8","46E9","46F0","46F1","46F2","46F3",
          "47E6","47E7","47E8","47E9","47F0","47F1","47F2","47F3",
          "48E6","48E7","48E8","48E9","48F0","48F1","48F2","48F3",
          "49E6","49E7","49E8","49E9","49F0","49F1","49F2","49F3",
          "50E6","50E7","50E8","50E9","50F0","50F1","50F2","50F3",
          "51E6","51E7","51E8","51E9","51F0","51F1","51F2","51F3",
          "52E6","52E7","52E8","52E9","52F0","52F1","52F2","52F3")

WoS <- c("38D8","38D9","38E0","38E1","38E2",
         "39D8","39D9","39E0","39E1","39E2","39E3","39E4","39E5",
         "40D8","40D9","40E0","40E1","40E2","40E3","40E4","40E5",
         "41D8","41D9","41E0","41E1","41E2","41E3","41E4","41E5",
         "42D8","42D9","42E0","42E1","42E2","42E3","42E4",
         "43D8","43D9","43E0","43E1","43E2","43E3","43E4",
         "44D8","44D9","44E0","44E1","44E2","44E3","44E4",
         "45D8","45D9","45E0","45E1","45E2","45E3","45E4","45E5",
         "46D8","46D9","46E0","46E1","46E2","46E3","46E4","46E5",
         "47D8","47D9","47E0","47E1","47E2","47E3","47E4","47E5",
         "48D8","48D9","48E0","48E1","48E2","48E3","48E4","48E5",
         "49E5")

Skg <- c("42F9",
         "43F8","43F9",
         "44F8","44F9","44G0","44G1",
         "45F9","45G0","45G1",
         "46G0","46G1")


#Assign spatial groups as determined in Holmes et al. 2014 (Table 2: East, West)
NSea.df   <- data.frame("ICESrec" = NSea)
WoS.df    <- data.frame("ICESrec" = WoS)
Skg.df    <- data.frame("ICESrec" = Skg)

spatial <- do.call("rbind", list(NSea.df, WoS.df, Skg.df))

#Get coordinates of ICES rectangles and assign east/west region
lonlat <- ices.rect(spatial$ICESrec) 
region <- cbind(spatial,lonlat)

east.area <- region[region$lon > 0,]
west.area <- region[region$lon < 0,]
east <- as.character(east.area$ICESrec)
west <- as.character(west.area$ICESrec)

#Some areas do not fall within the limits of the North Shelf: prepare to remove
Chann  <- c("30E8","30E9","30F0","30F1","29E8","29E9","29F0","29F1","28E8","28E9",
            "28F0","28F1","27E8","27E9","27F0","26E8")
Celtic <- c("39E6","38E5","38E6","37E6","37E7","36E6","36E7")
NorSka <- c("45F8","46F9","47G0")

# WGNSSK 2025 correction to historic data
haddock[[1]] <- haddock[[1]][!(haddock[[1]]$Year==2001 & haddock[[1]]$Age==1 & haddock[[1]]$LngtClas==42), ]

# save
save(list=c("haddock","east","west","Chann","Celtic","NorSka"),file="data/pre-processed survey data.RData")


