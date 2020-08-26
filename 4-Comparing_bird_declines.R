####Script 4####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Comparing caclulated and published percent declines for birds###

###start next stage by clearing environment 
rm(list=ls())

#install packages - unhash if unistalled
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

#set working direcotry (left blank for reader to include their own directory).
setwd()

#check working director
getwd()
dir()

#import useful datasets
CalcTrends <- read.csv("Aves_Trend_GL.csv", header = TRUE, row.names = 1)
PubTrends <- read.csv("PopTrend_Aves.csv", header = TRUE)

###data exploration 
##histograms
##for min
#caclulated
hist(CalcTrends$PercentDecrease10YMin)
hist(CalcTrends$PercentDecrease3GMin)
hist(CalcTrends$PercentDecreaseMin)
#published
hist(PubTrends$Optimistic.decline10years)
hist(PubTrends$Optimistic.decline3gens)

#for mid
#caclulated
hist(CalcTrends$PercentDecrease10YMid)
hist(CalcTrends$PercentDecrease3GMid)
hist(CalcTrends$PercentDecreaseMid)
#published
hist(PubTrends$Median.decline10years)
hist(PubTrends$Median.decline3gens)

#for max
#caclulated
hist(CalcTrends$PercentDecrease10YMax)
hist(CalcTrends$PercentDecrease3GMax)
hist(CalcTrends$PercentDecreaseMax)
#published
hist(PubTrends$Pessimistic.decline10years)
hist(PubTrends$Pessimistic.decline3gens)

#some data handling to run tests
#first select useful columns only 
CalcTrends <- select(CalcTrends, Scientific_Name, PercentDecrease10YMin, PercentDecrease3GMin, PercentDecreaseMin, PercentDecrease10YMid, PercentDecrease3GMid, PercentDecreaseMid,PercentDecrease10YMax, PercentDecrease3GMax, PercentDecreaseMax)
PubTrends <- select(PubTrends, Scientific.name, Optimistic.decline10years, Optimistic.decline3gens, Median.decline10years, Median.decline3gens, Pessimistic.decline10years, Pessimistic.decline3gens)

#set PubTrends names to match caltrends
colnames(PubTrends) <- c("Scientific_Name", "Optimistic.decline10years", "Optimistic.decline3gens", "Median.decline10years", "Median.decline3gens", "Pessimistic.decline10years", "Pessimistic.decline3gens")

#merge both dataframes
Trends <- merge(CalcTrends, PubTrends, all = TRUE, by = "Scientific_Name")

####Pearson's product-moment correlation test####

##for 10 years 
#min
cor.test(Trends$PercentDecrease10YMin, Trends$Optimistic.decline10years)
#0.577 95%CL 0.50-0.64

#mid
cor.test(Trends$PercentDecrease10YMid, Trends$Median.decline10years)
#0.781 95%CL 0.73-0.82

#max
cor.test(Trends$PercentDecrease10YMax, Trends$Pessimistic.decline10years)
#0.824 95%CL 0.79-0.86


##for three generations 
#min
cor.test(Trends$PercentDecrease3GMin, Trends$Optimistic.decline3gen)
#0.34 95%CL 0.24-0.43

#mid
cor.test(Trends$PercentDecrease3GMid, Trends$Median.decline3gens)
#0.36 95%CL 0.26-0.45

#max
cor.test(Trends$PercentDecrease3GMax, Trends$Pessimistic.decline3gens)
#0.47 95%CL 0.38-0.55


##for our best guess (10 years or 3 gen, whichever is longer) with their 10 years
#min
cor.test(Trends$PercentDecreaseMin, Trends$Optimistic.decline10years)
#0.803 95%CL 0.76-0.84

#mid
cor.test(Trends$PercentDecreaseMid, Trends$Median.decline10years)
#0.763 95%CL 0.71-0.80

#max
cor.test(Trends$PercentDecreaseMax, Trends$Pessimistic.decline10years)
#0.742 95%CL 0.69-0.79


##for our best guess (10 years or 3 gen, whichever is longer) with their 3 generations
#min
cor.test(Trends$PercentDecreaseMin, Trends$Optimistic.decline3gens)
#0.41 95%CL 0.32-0.50

#mid
cor.test(Trends$PercentDecreaseMid, Trends$Median.decline3gens)
#0.46 95%CL 0.37-0.54

#max
cor.test(Trends$PercentDecreaseMax, Trends$Pessimistic.decline3gens)
#0.56 95%CL 0.48-0.63

####fit simple linear model#### - no models were presented as they violated the normal 
#distribution of residuals assumptions and didn't give more infomraiton than the correlation tests.

###for 10 years 
##min
#create model
Mod10YMin <- lm(Trends$PercentDecrease10YMin ~ Trends$Optimistic.decline10years)
summary(Mod10YMin)
#plot model
plot(Trends$PercentDecrease10YMin ~ Trends$Optimistic.decline10years)
abline(lm(Trends$PercentDecrease10YMin ~ Trends$Optimistic.decline10years))

##mid
#create model
Mod10YMid <- lm(Trends$PercentDecrease10YMid ~ Trends$Median.decline10years)
summary(Mod10YMid)
#plot model
plot(PercentDecrease10YMid ~ Median.decline10years, data = Trends)
abline(lm(Trends$PercentDecrease10YMid ~ Trends$Median.decline10years))

##max
#create model
Mod10YMax <- lm(Trends$PercentDecrease10YMax ~ Trends$Pessimistic.decline10years)
summary(Mod10YMax)
#plot model
plot(PercentDecrease10YMax ~ Median.decline10years, data = Trends)
abline(lm(Trends$PercentDecrease10YMax ~ Trends$Pessimistic.decline10years))

###for three generations 
##min
#create model
Mod3GMin <- lm(Trends$PercentDecrease3GMin ~ Trends$Optimistic.decline3gens)
summary(Mod3GMin)
#plot model
plot(Trends$PercentDecrease3GMin ~ Trends$Optimistic.decline3gens, xlim = c(-0.3, 1))
abline(lm(Trends$PercentDecrease3GMin ~ Trends$Optimistic.decline3gens))

##mid
#create model
Mod3GMid <- lm(Trends$PercentDecrease3GMid ~ Trends$Median.decline3gens)
summary(Mod3GMid)
#plot model
plot(Trends$PercentDecrease3GMid ~ Trends$Median.decline3gens, xlim = c(0, 1))
abline(lm(Trends$PercentDecrease3GMid ~ Trends$Median.decline3gens))

##max
#create model
Mod3GMax <- lm(Trends$PercentDecrease3GMax ~ Trends$Pessimistic.decline3gens)
summary(Mod3GMax)
#plot model
plot(Trends$PercentDecrease3GMax ~ Trends$Pessimistic.decline3gens, xlim = c(0, 1))
abline(lm(Trends$PercentDecrease3GMax ~ Trends$Pessimistic.decline3gens))

###for our best guess (10 years or 3 gen, whichever is longer) with their 10 years
##min
#create model
ModBG10Min <- lm(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline10years)
summary(ModBG10Min)
#plot model
plot(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline10years, xlim = c(0, 1))
abline(lm(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline10years))
plot(ModBG10Min)

##mid 
#histograms for distribution
hist(Trends$PercentDecreaseMid)
hist(Trends$Median.decline10years)
#create model
ModBG10Mid <- lm(Trends$PercentDecreaseMid ~ Trends$Median.decline10years)
summary(ModBG10Mid)
#plot model
plot(jitter((Trends$PercentDecreaseMid)) ~ Trends$Median.decline10years, xlim = c(0, 1), xlab = "Published annual bird population declines", ylab = "Calculated annual percent bird population declines (%)", pch = 1, cex = 0.9)
abline(lm(Trends$PercentDecreaseMid ~ Trends$Median.decline10years))
plot(ModBG10Mid)

##TO BE PRESENTED IN THE REPORT 
#Bird trends are given in proportions where 1 = 100% pop decline and 0 = stabel population
#Therefore need to transform our data to be proportions to for correct comparisons etc. 
#histograms for distribution
hist(Trends$PercentDecreaseMid/100)
hist(Trends$Median.decline10years)
#create model
ModBG10Mid <- lm((Trends$PercentDecreaseMid/100) ~ Trends$Median.decline10years)
summary(ModBG10Mid)
#plot model
par(mgp=c(3,1,0))
plot(jitter(Trends$PercentDecreaseMid/100) ~ Trends$Median.decline10years, xlim = c(0, 1), xlab = "Published annual bird population declines (proportions)", ylab = "Calculated annual bird population\ndeclines (proportions)", pch = 1, cex = 0.7, las = 1, bty="l"   )
abline(lm((Trends$PercentDecreaseMid/100) ~ Trends$Median.decline10years))
plot(ModBG10Mid)


##max
#create model
ModBG10Max <- lm(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline10years)
summary(ModBG10Max)
#plot model
plot(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline10years, xlim = c(0, 1))
abline(lm(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline10years))


###for our best guess (10 years or 3 gen, whichever is longer) with their 3 generations
##min
#create model
ModBG3Min <- lm(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline3gens)
summary(ModBG3Min)
#plot model
plot(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline3gens, xlim = c(0, 1))
abline(lm(Trends$PercentDecreaseMin ~ Trends$Optimistic.decline3gens))

##mid
#create model
ModBG3Mid <- lm(Trends$PercentDecreaseMid ~ Trends$Median.decline3gens)
summary(ModBG3Mid)
#plot model
plot(Trends$PercentDecreaseMid ~ Trends$Median.decline3gens, xlim = c(0, 1))
abline(lm(Trends$PercentDecreaseMid ~ Trends$Median.decline3gens))

##max
#create model
ModBG3Max <- lm(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline3gens)
summary(ModBG3Max)
#plot model
plot(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline3gens, xlim = c(0, 1))
abline(lm(Trends$PercentDecreaseMax ~ Trends$Pessimistic.decline3gens))


#####BIRD COMPARISONS WITH ORIGINAL LPI LAMBDAS##### - this was not presented, it was only explored out of curiosity

###start next stage by clearing environment 
rm(list=ls())

#load specific packages
require(sjmisc)

#import useful datasets
LPITrends <- read.csv("merged_data_20191205_final_nonconf_poplambdas.csv", header = TRUE, row.names = 1)
PubTrends <- read.csv("PopTrend_Aves.csv", header = TRUE)

#create column in LPILambdas that states the number of years data were collected for 
LPITrends <- row_count(LPITrends, X1971.y:X2015.y, count = NA, append = TRUE)
LPITrends <- mutate(LPITrends, Length = (44 - rowcount))

##some data handling to run tests
#first select useful columns only 
LPITrends <- select(LPITrends, Binomial, av_lambda, sum_lambda, Length, Percentage)
PubTrends <- select(PubTrends, Scientific.name, Optimistic.decline10years, Optimistic.decline3gens, Median.decline10years, Median.decline3gens, Pessimistic.decline10years, Pessimistic.decline3gens)

#set PubTrends names to match LPITrends
colnames(PubTrends) <- c("Scientific_Name", "Optimistic.decline10years", "Optimistic.decline3gens", "Median.decline10years", "Median.decline3gens", "Pessimistic.decline10years", "Pessimistic.decline3gens")

#set LPITrends names to match PubTrends
colnames(LPITrends) <- c("Scientific_Name", "av_lambda", "sum_lambda", "Length", "Percentage_incorporated")

#change PubTrends Scientific_Name formatting to suit LPITrends
PubTrends$Scientific_Name <- sub(" ", "_", PubTrends$Scientific_Name)

#merge both dataframes
Trends <- merge(LPITrends, PubTrends, all = TRUE, by = "Scientific_Name")


####Pearson's product-moment correlation test#### - always using median published estimates
###Published trends 10 Year
##with av_lambda
cor.test(Trends$av_lambda, Trends$Median.decline10years)
#-0.214 95%CL -0.24 -> -0.19

##with sum_lambda
cor.test(Trends$sum_lambda, Trends$Median.decline10years)
#-0.237 -0.26 -> -0.21

###Published trends 3 Gens
##with av_lambda
cor.test(Trends$av_lambda, Trends$Median.decline3gens)
#-0.18 95%CL -0.21 -> -0.16

##with sum_lambda
cor.test(Trends$sum_lambda, Trends$Median.decline3gens)
#-0.212 95%CL -0.23 -> -0.19


####simple linear models#### - always using median published trends
###Published trends 10 years
##with av_lambdas
#create model
Mod10YA <- lm(Trends$av_lambda ~ Trends$Median.decline10years)
summary(Mod10YA)
#plot model
plot(Trends$av_lambda ~ Trends$Median.decline10years)
abline(lm(Trends$av_lambda ~ Trends$Median.decline10years))

##with sum_lambda
#create model
Mod10YS <- lm(Trends$sum_lambda ~ Trends$Median.decline10years)
summary(Mod10YS)
#plot model
plot(Trends$sum_lambda ~ Trends$Median.decline10years)
abline(lm(Trends$sum_lambda ~ Trends$Median.decline10years))

###Published trends 3 gens
##with av_lambda
#create model
Mod3GA <- lm(Trends$av_lambda ~ Trends$Median.decline3gens)
summary(Mod3GA)
#plot model
plot(Trends$av_lambda ~ Trends$Median.decline3gens)
abline(lm(Trends$av_lambda ~ Trends$Median.decline3gens))

##with sum_lambda
#create model
Mod3GS <- lm(Trends$sum_lambda ~ Trends$Median.decline3gens)
summary(Mod3GS)
#plot model
plot(Trends$sum_lambda ~ Trends$Median.decline3gens)
abline(lm(Trends$sum_lambda ~ Trends$Median.decline3gens))

#####SUBSET BIRD COMPARISONS WITH LPI LAMBDAS#####

##Subset Trends to only species with long time series and a high proportion of the global population included
SubsetTrends <- filter(Trends, Length >= 20 & Percentage_incorporated == "76-100%")


####Pearson's product-moment correlation test#### 
###always using median published estimates and average lambdas as little difference between av and sum lambdas.
###Published trends 10 Years
cor.test(SubsetTrends$av_lambda, SubsetTrends$Median.decline10years)
#-0.108 95%CL -0.32 -> -0.12

###Published trends 3 Gens
cor.test(SubsetTrends$av_lambda, SubsetTrends$Median.decline3gens)
#-0.085 95%CL -0.30 -> -0.13

####simple linear models#### 
###always using median published estimates and average lambdas as little difference between av and sum lambdas.
###Published trends 10 years
#create model
ModSub10YA <- lm(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline10years)
summary(ModSub10YA)
#plot model
plot(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline10years)
abline(lm(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline10years))

###Published trends 3 gens
#create model
ModSub3GA <- lm(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline3gens)
summary(SubMod3GA)
#plot model
plot(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline3gens)
abline(lm(SubsetTrends$av_lambda ~ SubsetTrends$Median.decline3gens))
