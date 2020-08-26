####Script 7####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Summary stats for results section###

###start next stage by clearing environment 
rm(list=ls())

#install packages - unhash if ever unistalled
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
#install.packages("rredlist")
#devtools::install_github("ropenscilabs/rredlist")

#load packages
library(plyr)
library(dplyr)
require(stringr)
require(naniar)
require(devtools)
require(rlpi)
require(rredlist)
require(tidyverse)
require(car)
require(ggplot2)

#set working direcotry (left blank for reader to include their own directory).
setwd()

#check working director
getwd()
dir()

#import useful datasets
LPIandCalcTrends <- read.csv("CalculatedLambdas_Either.csv", header = TRUE, row.names = 1)
Aves_Trend_GL <- read.csv("CalcLambdasThreatenedAves.csv", header = TRUE, row.names = 1)
Mammals_Trend_GL <- read.csv("CalcLambdasThreatenedMams.csv", header = TRUE, row.names = 1) 
OurTrends <- read.csv("Data_With_Stable_Trends.csv", header = TRUE, row.names = 1) 

#Subset to only species were a trend could be calculated 
Data <- OurTrends%>%filter(!is.na(PercentDecrease10YMin))

write.csv(Data, "OnlyCalculatedTrends.csv")

#number of species with IUCN criteria i.e. threatened species
length(OurTrends$IUCN_Criteria) - sum(is.na(OurTrends$IUCN_Criteria))

#number of species classified under Crit A2 and not in the LPI
CritA2 <- Data[grep('A2', Data$IUCN_Criteria), ]
sum(CritA2$PresentInRL) - sum(CritA2$PresentInLPI)

#number of species listed as stable and not in the LPI
StableSpecies <- filter(Data, Population_Trend %in% "Stable" & PresentInLPI %in% FALSE & PercentDecrease10YMin %in% -2.5)
nrow(StableSpecies)

#function to allow calculation or standard error
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#best guest mean rate of decrease, SD, SE and sample size for all vertebrates (including stable populations) - using 10 years or 3 gens whichever longer, or 10 years when no gen data is available. 
#min
mean(Data$PercentDecreaseMin, na.rm = TRUE)
log(sd(Data$PercentDecreaseMin, na.rm = TRUE))
log(se(Data$PercentDecreaseMin, na.rm = TRUE))
length(which(!is.na(Data$PercentDecreaseMin)))
##PRESENTED - mid
mean(Data$PercentDecreaseMid, na.rm = TRUE)
log(sd(Data$PercentDecreaseMid, na.rm = TRUE))
log(se(Data$PercentDecreaseMid, na.rm = TRUE))
length(which(!is.na(Data$PercentDecreaseMid)))
#max
mean(Data$PercentDecreaseMax, na.rm = TRUE)
log(sd(Data$PercentDecreaseMax, na.rm = TRUE))
log(se(Data$PercentDecreaseMax, na.rm = TRUE))
length(which(!is.na(Data$PercentDecreaseMax)))

###Subset to only bird species
DataAves <- filter(Data, Class %in% "aves")
##Mean rate of decrease SE and sample size for IUCN birds (including stable populations) - using mid estimate calculated using 10 Years or 3 Gens
mean(DataAves$PercentDecreaseMid)
log(se(DataAves$PercentDecreaseMid))
length(which(!is.na(DataAves$PercentDecreaseMid)))
#number species with 3 GL > 10 Y
length(which((3 * DataAves$Generation_Length_Years > 10)))

###Subset to only mammal species
DataMams <- filter(Data, Class %in% "mammalia")
##Mean rate of decrease SE and sample size for IUCN mams (including stable populations) - using mid estimate calculated using 10 Years or 3 Gens
mean(DataMams$PercentDecreaseMid)
log(se(DataMams$PercentDecreaseMid))
length(which(!is.na(DataMams$PercentDecreaseMid)))
#number species with 3 GL > 10 Y
length(which((3 * DataMams$Generation_Length_Years > 10)))


###Subset to only threatened IUCN vertebrates to complete same calcualtions 
ThreatenedSpecies <- filter(Data, IUCN_Category %in% c("CR", "VU", "EN"))

##mean rate of decrease per annum, SD, SE and sample size for threatened IUCN vertebrates (including stable) - calculated using 10 years or 3 gens, whichever longer
#min
mean(ThreatenedSpecies$PercentDecreaseMin, na.rm = TRUE)
log(sd(ThreatenedSpecies$PercentDecreaseMin, na.rm = TRUE))
log(se(ThreatenedSpecies$PercentDecreaseMin, na.rm = TRUE))
length(which(!is.na(ThreatenedSpecies$PercentDecreaseMin)))
#mid - PRESENTED
mean(ThreatenedSpecies$PercentDecreaseMid, na.rm = TRUE)
log(sd(ThreatenedSpecies$PercentDecreaseMid, na.rm = TRUE))
log(se(ThreatenedSpecies$PercentDecreaseMid, na.rm = TRUE))
length(which(!is.na(ThreatenedSpecies$PercentDecreaseMid)))
#max
mean(ThreatenedSpecies$PercentDecreaseMax, na.rm = TRUE)
log(sd(ThreatenedSpecies$PercentDecreaseMax, na.rm = TRUE))
log(se(ThreatenedSpecies$PercentDecreaseMax, na.rm = TRUE))
length(which(!is.na(ThreatenedSpecies$PercentDecreaseMid)))


#mean rate of decrease per annum, SD, SE and sample size for threatened ICUN Mammals (not including stable populations) - calculated using 10 years or 3 gens whichever longer
#min
mean(Mammals_Trend_GL$PercentDecreaseMin)
log(sd(Mammals_Trend_GL$PercentDecreaseMin))
log(se(Mammals_Trend_GL$PercentDecreaseMin))
length(which(!is.na(Mammals_Trend_GL$PercentDecreaseMin)))
#mid - PRESENTED
mean(Mammals_Trend_GL$PercentDecreaseMid)
log(sd(Mammals_Trend_GL$PercentDecreaseMid))
log(se(Mammals_Trend_GL$PercentDecreaseMid))
length(which(!is.na(Mammals_Trend_GL$PercentDecreaseMid)))
#mix
mean(Mammals_Trend_GL$PercentDecreaseMax)
log(sd(Mammals_Trend_GL$PercentDecreaseMax))
log(se(Mammals_Trend_GL$PercentDecreaseMax))
length(which(!is.na(Mammals_Trend_GL$PercentDecreaseMax)))

#mean rate of decrease per annum, SD, SE and sample size for threatened ICUN Birds (not including stable populations) - calculated using 10 years or 3 gens whichever longer
#min
mean(Aves_Trend_GL$PercentDecreaseMin)
log(sd(Aves_Trend_GL$PercentDecreaseMin))
log(se(Aves_Trend_GL$PercentDecreaseMin))
length(which(!is.na(Aves_Trend_GL$PercentDecreaseMin)))
#mid - PRESENTED
mean(Aves_Trend_GL$PercentDecreaseMid)
log(sd(Aves_Trend_GL$PercentDecreaseMid))
log(se(Aves_Trend_GL$PercentDecreaseMid))
length(which(!is.na(Aves_Trend_GL$PercentDecreaseMid)))
#mix
mean(Aves_Trend_GL$PercentDecreaseMax)
log(sd(Aves_Trend_GL$PercentDecreaseMax))
log(se(Aves_Trend_GL$PercentDecreaseMax))
length(which(!is.na(Aves_Trend_GL$PercentDecreaseMax)))

#mean rate of decrease per annum, SD and SE for the 1,109 threatened IUCN vertebrates that are not in the LPI - calculated using 10 years or 3 gens whichever longer
DecreaseNotInLPI <- Data%>%filter(PresentInLPI %in% FALSE)
#min
mean(DecreaseNotInLPI$PercentDecreaseMin, na.rm = TRUE)
log(sd(DecreaseNotInLPI$PercentDecreaseMin, na.rm = TRUE))
log(se(DecreaseNotInLPI$PercentDecreaseMin, na.rm = TRUE))
#mid
mean(DecreaseNotInLPI$PercentDecreaseMid, na.rm = TRUE)
log(sd(DecreaseNotInLPI$PercentDecreaseMid, na.rm = TRUE))
log(se(DecreaseNotInLPI$PercentDecreaseMid, na.rm = TRUE))
#mix
mean(DecreaseNotInLPI$PercentDecreaseMax, na.rm = TRUE)
log(sd(DecreaseNotInLPI$PercentDecreaseMax, na.rm = TRUE))
log(se(DecreaseNotInLPI$PercentDecreaseMax, na.rm = TRUE))

###mean lambda, SD and SE for all vertebrates - calculated using 10 years or 3 gens whichever longer 
#min
mean(LPIandCalcTrends$LambdaMin, na.rm = TRUE)
log(se(LPIandCalcTrends$LambdaMin, na.rm = TRUE))
log(sd(LPIandCalcTrends$LambdaMin, na.rm = TRUE))
#mid
mean(LPIandCalcTrends$LambdaMid, na.rm = TRUE)
log(se(LPIandCalcTrends$LambdaMid, na.rm = TRUE))
log(sd(LPIandCalcTrends$LambdaMid, na.rm = TRUE))
#max
mean(LPIandCalcTrends$LambdaMax, na.rm = TRUE)
log(se(LPIandCalcTrends$LambdaMax, na.rm = TRUE))
log(sd(LPIandCalcTrends$LambdaMax, na.rm = TRUE))
