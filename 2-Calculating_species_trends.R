####Script 2####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Calculating a percent decrease per annaun for each species###
#on final dataframe at end of script (Data_Trend_GL) percent decrease Min/Mid/Max columns are our best guess, inlcudes all stable populations, calculations for non bird/mammal species using just 10 years and caulcations for bird/mammals species using 10 years or 3 gen whatever longer

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

#check working directory
getwd()
dir()

#import useful dataset
DataWithCriteria <- read.csv("DataWithCriteria&Trend.csv", header = TRUE, row.names = 1)

#create new dataset with only species categorised by criterion A2
DataCritA2 <-  DataWithCriteria[grep('A2', DataWithCriteria$IUCN_Criteria), ]

#some species listed more than once - belive this is because different populations of some species (e.g. turtles) are in the RL, some non-threatened species have criterion (maybe because other populalations that are threated have criteria)
DataCritA2 <- DataCritA2[!duplicated(DataCritA2[,'Scientific_Name']),]

##how many of these not already in LPI - for results section
sum(DataCritA2$PresentInLPI %in% FALSE)

#for CR species create a percent decrease column for min, mid and max values. Fill with population decline calculated from iucn categories 
DataCritA2$PercentDecreaseMin[DataCritA2$IUCN_Category %in% "CR"] <- (1 - 10^(log10(0.20)/10))*100 #80
DataCritA2$PercentDecreaseMid[DataCritA2$IUCN_Category %in% "CR"] <- (1 - 10^(log10(0.105)/10))*100#89.5
DataCritA2$PercentDecreaseMax[DataCritA2$IUCN_Category %in% "CR"] <- (1 - 10^(log10(0.01)/10))*100 #99

#for EN species create a percent decrease column for min, mid and max values. Fill with population decline from iucn categories 
DataCritA2$PercentDecreaseMin[DataCritA2$IUCN_Category %in% "EN"] <- (1 - 10^(log10(0.50)/10))*100 #50
DataCritA2$PercentDecreaseMid[DataCritA2$IUCN_Category %in% "EN"] <- (1 - 10^(log10(0.355)/10))*100 #64.5
DataCritA2$PercentDecreaseMax[DataCritA2$IUCN_Category %in% "EN"] <- (1 - 10^(log10(0.21)/10))*100 #79

#for VU species create a percent decrease column for min, mid and max values. Fill with population decline from iucn categories 
DataCritA2$PercentDecreaseMin[DataCritA2$IUCN_Category %in% "VU"] <- (1 - 10^(log10(0.70)/10))*100  #30
DataCritA2$PercentDecreaseMid[DataCritA2$IUCN_Category %in% "VU"] <- (1 - 10^(log10(0.605)/10))*100 #39.5
DataCritA2$PercentDecreaseMax[DataCritA2$IUCN_Category %in% "VU"] <- (1 - 10^(log10(0.51)/10))*100  #49

#add the same values for PercenDecrease10YMin/Mid/Max - These are the trends when calcualted using 10 years only, later the trends in PercentDecreaseMin/Mid/Max will be altered to include generation length for species with 3*GL > 10
#for Min
DataCritA2$PercentDecrease10YMin <-  DataCritA2$PercentDecreaseMin
#for Mid
DataCritA2$PercentDecrease10YMid <-  DataCritA2$PercentDecreaseMid
#for Max
DataCritA2$PercentDecrease10YMax <-  DataCritA2$PercentDecreaseMax

#check structure of Data_CritA2 and correct variable types
str(DataCritA2)
DataCritA2$Scientific_Name <- as.character(DataCritA2$Scientific_Name)
DataCritA2$Common_Name <- as.character(DataCritA2$Common_Name)
str(DataCritA2)

#check which species not included 
Check1 <- filter(DataCritA2, !(IUCN_Category %in% c("CR", "VU", "EN")))

###Alter trend to include species generation lengths for mammals and birds in the caluclation###

#import useful datasets
dir()
GL_Mammals <- read.csv("GL_Mammals.csv", header = TRUE)
GL_Aves <- read.csv("GL_Aves.csv", header = TRUE)

###tidy new datasets
##GL_Mammals 
#select useful columns
GL_Mammals <- select(GL_Mammals, Scientific_name, GenerationLength_d)
colnames(GL_Mammals) <- c("Scientific_Name", "Generation_Length_Days")

#change Scientific Name to character
str(GL_Mammals)
GL_Mammals$Scientific_Name <- as.character(GL_Mammals$Scientific_Name)
str(GL_Mammals)

#sort alphabetically by Scientific Name
GL_Mammals <- arrange(GL_Mammals, Scientific_Name)

#add a column for generation length in years
GL_Mammals<- mutate(GL_Mammals, Generation_Length_Years = Generation_Length_Days/365.25)

##GL_Aves
#select useful columns
GL_Aves <- select(GL_Aves, Scientific.name, GenLength)
colnames(GL_Aves) <- c("Scientific_Name", "Generation_Length_Years")

#Add a column for generation length in days
GL_Aves <- mutate(GL_Aves, Generation_Length_Days = Generation_Length_Years * 365.25)

##merge generation length dataframes for birds and mammals with dataframe with population trends for IUCN threatened vertebrates listed under Criterion A
Mammals_Trend_GL <- merge(GL_Mammals, DataCritA2, all = FALSE, by = "Scientific_Name")
Aves_Trend_GL <- merge(GL_Aves, DataCritA2, all = FALSE, by = "Scientific_Name")

####Alter popuation decrease depending on generation lengths
###for mammals

##for Min column
#make a column called rate depending on iucn category
Mammals_Trend_GL$rateMin[Mammals_Trend_GL$IUCN_Category == 'CR'] <- log10(0.20)  #80
Mammals_Trend_GL$rateMin[Mammals_Trend_GL$IUCN_Category == 'EN'] <- log10(0.50)  #50
Mammals_Trend_GL$rateMin[Mammals_Trend_GL$IUCN_Category == 'VU'] <- log10(0.70)  #30
#create a column for trends calculted using 3 generations
Mammals_Trend_GL$PercentDecrease3GMin <-  100*(1-10^(Mammals_Trend_GL$rateMin / (Mammals_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Mammals_Trend_GL$PercentDecreaseMin[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10] <- Mammals_Trend_GL$PercentDecrease3GMin[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

##for Mid column 
#make a column called rate depending on iucn category
Mammals_Trend_GL$rateMid[Mammals_Trend_GL$IUCN_Category == 'CR'] <- log10(0.105)  #89.5
Mammals_Trend_GL$rateMid[Mammals_Trend_GL$IUCN_Category == 'EN'] <- log10(0.355)  #64.5
Mammals_Trend_GL$rateMid[Mammals_Trend_GL$IUCN_Category == 'VU'] <- log10(0.605)  #39.5
#create a column for trends calculted using 3 generations
Mammals_Trend_GL$PercentDecrease3GMid <-  100*(1-10^(Mammals_Trend_GL$rateMid / (Mammals_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Mammals_Trend_GL$PercentDecreaseMid[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10] <- Mammals_Trend_GL$PercentDecrease3GMid[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

##for Max column
#make a column called rate depending on iucn category
Mammals_Trend_GL$rateMax[Mammals_Trend_GL$IUCN_Category == 'CR'] <- log10(0.01)  #99
Mammals_Trend_GL$rateMax[Mammals_Trend_GL$IUCN_Category == 'EN'] <- log10(0.21)  #79
Mammals_Trend_GL$rateMax[Mammals_Trend_GL$IUCN_Category == 'VU'] <- log10(0.51)  #49
#create a column for trends calculted using 3 generations
Mammals_Trend_GL$PercentDecrease3GMax <-  100*(1-10^(Mammals_Trend_GL$rateMax / (Mammals_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Mammals_Trend_GL$PercentDecreaseMax[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10] <- Mammals_Trend_GL$PercentDecrease3GMax[(Mammals_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

#remove rate columns
Mammals_Trend_GL <- select(Mammals_Trend_GL, -rateMin, -rateMid, -rateMax)

#reorder columns
Mammals_Trend_GL <- Mammals_Trend_GL[, c(1, 10, 4, 5, 7, 9, 6, 8, 2, 3, 14, 11, 17, 15, 12, 18, 16, 13, 19)]

##for birds
##for Min column
#make a column called rate depending on iucn category
Aves_Trend_GL$rateMin[Aves_Trend_GL$IUCN_Category == 'CR'] <- log10(0.20)  #80
Aves_Trend_GL$rateMin[Aves_Trend_GL$IUCN_Category == 'EN'] <- log10(0.50)  #50
Aves_Trend_GL$rateMin[Aves_Trend_GL$IUCN_Category == 'VU'] <- log10(0.70)  #30
#create a column for trends calculted using 3 generations
Aves_Trend_GL$PercentDecrease3GMin <-  100*(1-10^(Aves_Trend_GL$rateMin / (Aves_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Aves_Trend_GL$PercentDecreaseMin[(Aves_Trend_GL$Generation_Length_Years*3) >= 10] <- Aves_Trend_GL$PercentDecrease3GMin[(Aves_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

##for Mid column 
#make a column called rate depending on iucn category
Aves_Trend_GL$rateMid[Aves_Trend_GL$IUCN_Category == 'CR'] <- log10(0.105)  #89.5
Aves_Trend_GL$rateMid[Aves_Trend_GL$IUCN_Category == 'EN'] <- log10(0.355)  #64.5
Aves_Trend_GL$rateMid[Aves_Trend_GL$IUCN_Category == 'VU'] <- log10(0.605)  #39.5
#create a column for trends calculted using 3 generations
Aves_Trend_GL$PercentDecrease3GMid <-  100*(1-10^(Aves_Trend_GL$rateMid / (Aves_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Aves_Trend_GL$PercentDecreaseMid[(Aves_Trend_GL$Generation_Length_Years*3) >= 10] <- Aves_Trend_GL$PercentDecrease3GMid[(Aves_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

##for Max column
#make a column called rate depending on iucn category
Aves_Trend_GL$rateMax[Aves_Trend_GL$IUCN_Category == 'CR'] <- log10(0.01)  #99
Aves_Trend_GL$rateMax[Aves_Trend_GL$IUCN_Category == 'EN'] <- log10(0.21)  #79
Aves_Trend_GL$rateMax[Aves_Trend_GL$IUCN_Category == 'VU'] <- log10(0.51)  #49
#create a column for trends calculted using 3 generations
Aves_Trend_GL$PercentDecrease3GMax <-  100*(1-10^(Aves_Trend_GL$rateMax / (Aves_Trend_GL$Generation_Length_Years*3)))
#for the PercentDecrease colunm alter those species with three times their generation length larger than 10 by replacing the value calculated using 10 years with the value calculated using 3 generations
Aves_Trend_GL$PercentDecreaseMax[(Aves_Trend_GL$Generation_Length_Years*3) >= 10] <- Aves_Trend_GL$PercentDecrease3GMax[(Aves_Trend_GL$Generation_Length_Years*3) >= 10]
#will result in three columns, one for calculations using 3 Gens, one for calulcuations using 10 years and one for calculations using 3G or 10Y whichever longer (PercentDecrease)

#remove rate columns
Aves_Trend_GL <- select(Aves_Trend_GL, -rateMin, -rateMid, -rateMax)

#reorder columns
Aves_Trend_GL <- Aves_Trend_GL[, c(1, 10, 4, 5, 7, 9, 6, 8, 2, 3, 14, 11, 17, 15, 12, 18, 16, 13, 19)]

#save output
write.csv(Mammals_Trend_GL, "Mammals_Trend_GL.csv")
write.csv(Aves_Trend_GL, "Aves_Trend_GL.csv")

#merge Aves and Mammal trend data with all data (DataWithCriteria) and dataCritaA using all = TRUE - this will also include threatened non bird/mammal species
Data_Trend_GL <- merge(DataCritA2, DataWithCriteria, all = TRUE, by = c("Scientific_Name", "Common_Name", "Class", "PresentInRL", "PresentInLPI", "Population_Trend", "IUCN_Category", "IUCN_Criteria"))
Data_Trend_GL <- merge(Mammals_Trend_GL, Data_Trend_GL, all = TRUE, by = c("Scientific_Name", "Common_Name", "Class", "PresentInRL", "PresentInLPI", "IUCN_Category", "Population_Trend", "IUCN_Criteria", "PercentDecrease10YMin", "PercentDecreaseMin", "PercentDecrease10YMid", "PercentDecreaseMid", "PercentDecrease10YMax", "PercentDecreaseMax"))
Data_Trend_GL <- merge(Aves_Trend_GL, Data_Trend_GL, all = TRUE, by = c("Scientific_Name", "Common_Name", "Class", "PresentInRL", "PresentInLPI", "IUCN_Category", "Population_Trend", "IUCN_Criteria", "Generation_Length_Years", "Generation_Length_Days", "PercentDecrease10YMin", "PercentDecreaseMin", "PercentDecrease3GMin", "PercentDecrease10YMid", "PercentDecreaseMid", "PercentDecrease3GMid", "PercentDecrease10YMax", "PercentDecreaseMax", "PercentDecrease3GMax"))

#run new dataframe through distinct
Data_Trend_GL <- distinct(Data_Trend_GL)

##for species with a stable population trend, sample a number between -2.5 and +2.5 for as their PercentDecreaseMid, use -2.5 for max and +2.5 for min 
Data_Trend_GL$PercentDecreaseSMin [Data_Trend_GL$Population_Trend == "Stable"] <- -2.5
Numbers <- runif(sum(Data_Trend_GL$Population_Trend %in% "Stable" & !is.na(Data_Trend_GL$Population_Trend)), -2.5, 2.5)
Data_Trend_GL$PercentDecreaseSMid [Data_Trend_GL$Population_Trend == "Stable" & !is.na(Data_Trend_GL$Population_Trend)] <- Numbers
Data_Trend_GL$PercentDecreaseSMax [Data_Trend_GL$Population_Trend == "Stable"] <- 2.5

#check structure of Data_Trend_GL and turn PercentDecreaseSMid to numeric
str(Data_Trend_GL)
Data_Trend_GL$PercentDecreaseSMid <- as.numeric(Data_Trend_GL$PercentDecreaseSMid)
str(Data_Trend_GL)

#coalese with percentage decrease ensuring that new trend does not overwrite old trends, just fills where there is currently a gap.
#For Min Columns
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecreaseMin = coalesce(PercentDecreaseMin, PercentDecreaseSMin))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease10YMin = coalesce(PercentDecrease10YMin, PercentDecreaseSMin))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease3GMin = coalesce(PercentDecrease3GMin, PercentDecreaseSMin))

#For Mid Columns
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecreaseMid = coalesce(PercentDecreaseMid, PercentDecreaseSMid))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease10YMid = coalesce(PercentDecrease10YMid, PercentDecreaseSMid))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease3GMid = coalesce(PercentDecrease3GMid, PercentDecreaseSMid))

#For Mid Columns
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecreaseMax = coalesce(PercentDecreaseMax, PercentDecreaseSMax))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease10YMax = coalesce(PercentDecrease10YMax, PercentDecreaseSMax))
Data_Trend_GL <- Data_Trend_GL%>%mutate(PercentDecrease3GMax = coalesce(PercentDecrease3GMax, PercentDecreaseSMax))

#remove SMin, SMid and SMax columns
Data_Trend_GL <- select(Data_Trend_GL, -c(PercentDecreaseSMin, PercentDecreaseSMid, PercentDecreaseSMax))

#reorder columns
Data_Trend_GL <- Data_Trend_GL[, c(1, 2, 3, 4, 5, 7, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]

#remove duplicates
Data_Trend_GL <- Data_Trend_GL[!duplicated(Data_Trend_GL[,c('Scientific_Name', 'Common_Name', 'Class')]),]

#save output for all data
write.csv(Data_Trend_GL, "Data_With_Stable_Trends.csv")

