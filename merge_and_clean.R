st################################################################################
#Housekeeping!!
################################################################################

#Load required libraries - this list just keeps growing
library(epicalc)
library(foreign)
library(maxLik)
library(foreign)
library(MASS)
library(haven)

#Cleanup
rm(list=ls())                                  #remove all variables
detachAllData()                                #detach all previous data - requires epicalc library
setwd("~/Desktop/silly_warpaper/data")         #set working directory

#Fiddle with the data here
gwf_regimes <- read_dta("~/gwf_regimes/GWF_AllPoliticalRegimes.dta")
colnames(gwf_regimes)[1] <- "ccode"
save(GWF_AllPoliticalRegimes, file = "gwf_allpol.dta")
rm(list=ls())

strat_rival <- read.csv("strat_rival.csv",header = TRUE)
#making 2 "dubious" changes to variables since analysis will only be 1946 to 2005
#make prior to 1816 just 1816 for this coding. Won't matter
#make "ongoing" just 2010 for this, won't matter
levels(strat_rival$start)[97] <- "1816"
levels(strat_rival$end)[76] <- "2010"

#missing values are lack of rivalry. Set to zero
strat_rival[is.na(strat_rival)] <- 0

#turn end and start dates from factors to numeric values for analysis
strat_rival$end <- as.numeric(as.character(strat_rival$end))
strat_rival$start <- as.numeric(as.character(strat_rival$start))

#create prior year territory variable - need MID data first here - INCOMPLETE
strat_subset <- strat_rival[which(strat_rival$end>1944),]


#generate list of potential conflict statuses for every state and every year in rivalry data
rival_potential <- expand.grid(unique(c(strat_rival$ccode1, strat_rival$ccode2)), 1945:2010)
colnames(rival_potential) = c("ccode", "year")

#Generate lagged measure for territorial rivalry
#Takes a value of 1 if there is a state has territorial rivalary in the previous year
rival_potential$territory_lastyr <- NA
for (i in 1:nrow(rival_potential)){
  prior_year = rival_potential$year[i] - 1
  
  subset <- which((strat_rival$ccode1 == rival_potential$ccode[i] | strat_rival$ccode2 == rival_potential$ccode[i]) & 
                    strat_rival$start <= prior_year & strat_rival$end >= prior_year & strat_rival$Spatial == 1)
  
  rival_potential$territory_lastyr[i] <- ifelse(length(subset>0),1,0)
}

#Generate lagged measure for non-territorial rivalry
#Takes a value of 1 if there is a state has non-territorial rivalary in the previous year BUT NO territorial rivalary
rival_potential$nonterritory_lastyr <- NA
for (i in 1:nrow(rival_potential)){
  prior_year = rival_potential$year[i] - 1
  
  subset <- which((strat_rival$ccode1 == rival_potential$ccode[i] | strat_rival$ccode2 == rival_potential$ccode[i]) & 
                    strat_rival$start <= prior_year & strat_rival$end >= prior_year &
                    (strat_rival$Positional == 1 | strat_rival$Ideol == 1 | 
                      strat_rival$Interv == 1))
  rival_potential$nonterritory_lastyr[i] <- ifelse(length(subset) > 0 & sum(strat_rival$Spatial[subset]) == 0,1,0)
}

