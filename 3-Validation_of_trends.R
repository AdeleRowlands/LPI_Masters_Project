####Script 3####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Validating cacluated trends using current LPI trends###

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
require(sjmisc)
require(grDevices)

#set working direcotry (left blank for reader to include their own directory).
setwd()

#check working directory
getwd()
dir()

#import useful datasets
CalculatedTrends <- read.csv("Data_With_Stable_Trends.csv", header = TRUE, row.names = 1)
LPILambdas <- read.csv("merged_data_20191205_final_nonconf_poplambdas.csv", header = TRUE, row.names = 1)
MamsCalcTrends <- read.csv("Mammals_Trend_GL.csv", header = TRUE, row.names = 1)
AvesCalcTrends <- read.csv("Aves_Trend_GL.csv", header = TRUE, row.names = 1)

###converting our % declines to lambdas for comparison with LPI trends
##for whole dataset 
#for min estimates 
CalculatedTrends <- mutate(CalculatedTrends, Lambda10YMin = 1 - (PercentDecrease10YMin/100))
CalculatedTrends <- mutate(CalculatedTrends, LambdaMin = 1 - (PercentDecreaseMin/100))
CalculatedTrends <- mutate(CalculatedTrends, Lambda3GMin = 1 - (PercentDecrease3GMin/100))

#for mid estimates
CalculatedTrends <- mutate(CalculatedTrends, Lambda10YMid = 1 - (PercentDecrease10YMid/100))
CalculatedTrends <- mutate(CalculatedTrends, LambdaMid = 1 - (PercentDecreaseMid/100))
CalculatedTrends <- mutate(CalculatedTrends, Lambda3GMid = 1 - (PercentDecrease3GMid/100))

#for max estiamtes
CalculatedTrends <- mutate(CalculatedTrends, Lambda10YMax = 1 - (PercentDecrease10YMax/100))
CalculatedTrends <- mutate(CalculatedTrends, LambdaMax = 1 - (PercentDecreaseMax/100))
CalculatedTrends <- mutate(CalculatedTrends, Lambda3GMax = 1 - (PercentDecrease3GMax/100))


##for mammals
#for min estimates
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda10YMin = 1 - (PercentDecrease10YMin/100))
MamsCalcTrends <- mutate(MamsCalcTrends, LambdaMin = 1 - (PercentDecreaseMin/100))
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda3GMin = 1 - (PercentDecrease3GMin/100))

#for mid estimates
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda10YMid = 1 - (PercentDecrease10YMid/100))
MamsCalcTrends <- mutate(MamsCalcTrends, LambdaMid = 1 - (PercentDecreaseMid/100))
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda3GMid = 1 - (PercentDecrease3GMid/100))

#for max estiamtes
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda10YMax = 1 - (PercentDecrease10YMax/100))
MamsCalcTrends <- mutate(MamsCalcTrends, LambdaMax = 1 - (PercentDecreaseMax/100))
MamsCalcTrends <- mutate(MamsCalcTrends, Lambda3GMax = 1 - (PercentDecrease3GMax/100))


##for birds
#for min estimates
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda10YMin = 1 - (PercentDecrease10YMin/100))
AvesCalcTrends <- mutate(AvesCalcTrends, LambdaMin = 1 - (PercentDecreaseMin/100))
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda3GMin = 1 - (PercentDecrease3GMin/100))

#for mid estimates
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda10YMid = 1 - (PercentDecrease10YMid/100))
AvesCalcTrends <- mutate(AvesCalcTrends, LambdaMid = 1 - (PercentDecreaseMid/100))
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda3GMid = 1 - (PercentDecrease3GMid/100))

#for max estiamtes
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda10YMax = 1 - (PercentDecrease10YMax/100))
AvesCalcTrends <- mutate(AvesCalcTrends, LambdaMax = 1 - (PercentDecreaseMax/100))
AvesCalcTrends <- mutate(AvesCalcTrends, Lambda3GMax = 1 - (PercentDecrease3GMax/100))
  

###converting these lambdas to exponents 
##for whole dataset 
#for min estimates
CalculatedTrends <- mutate(CalculatedTrends, ExLambda10YMin = log(Lambda10YMin))
CalculatedTrends <- mutate(CalculatedTrends, ExLambdaMin = log(LambdaMin))
CalculatedTrends <- mutate(CalculatedTrends, ExLambda3GMin = log(Lambda3GMin))
#for mid estimates
CalculatedTrends <- mutate(CalculatedTrends, ExLambda10YMid = log(Lambda10YMid))
CalculatedTrends <- mutate(CalculatedTrends, ExLambdaMid = log(LambdaMid))
CalculatedTrends <- mutate(CalculatedTrends, ExLambda3GMid = log(Lambda3GMid))

#for max estiamtes
CalculatedTrends <- mutate(CalculatedTrends, ExLambda10YMax = log(Lambda10YMax))
CalculatedTrends <- mutate(CalculatedTrends, ExLambdaMax = log(LambdaMax))
CalculatedTrends <- mutate(CalculatedTrends, ExLambda3GMax = log(Lambda3GMax))

#save dataframe
write.csv(CalculatedTrends, "IUCN-derived-Lambdas&Declines.csv")

##for mammals
#for min estimates
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda10YMin = log(Lambda10YMin))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambdaMin = log(LambdaMin))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda3GMin = log(Lambda3GMin))

#for mid estimates
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda10YMid = log(Lambda10YMid))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambdaMid = log(LambdaMid))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda3GMid = log(Lambda3GMid))

#for max estiamtes
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda10YMax = log(Lambda10YMax))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambdaMax = log(LambdaMax))
MamsCalcTrends <- mutate(MamsCalcTrends, ExLambda3GMax = log(Lambda3GMax))


##for birds
#for min estimates
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda10YMin = log(Lambda10YMin))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambdaMin = log(LambdaMin))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda3GMin = log(Lambda3GMin))

#for mid estimates
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda10YMid = log(Lambda10YMid))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambdaMid = log(LambdaMid))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda3GMid = log(Lambda3GMid))

#for max estiamtes
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda10YMax = log(Lambda10YMax))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambdaMax = log(LambdaMax))
AvesCalcTrends <- mutate(AvesCalcTrends, ExLambda3GMax = log(Lambda3GMax))


####data exploration: frequency histograms
###LPI
#average exponential lambdas
hist(LPILambdas$av_lambda)
#total exponential lambdas
hist(LPILambdas$sum_lambda)

###Calculated Trends
#for mid
#caclulated verts
hist(CalculatedTrends$ExLambdaMid)
#caclulated birds
hist(AvesCalcTrends$ExLambdaMid)
#caclulated mammals
hist(MamsCalcTrends$ExLambdaMid)


###Some data handling to run tests###

#create column in LPILambdas that states the number of years data were collected for 
LPILambdas <- row_count(LPILambdas, X1971.y:X2015.y, count = NA, append = TRUE)
LPILambdas <- mutate(LPILambdas, Length = (44 - rowcount))

#Select useful columns only from LPI lambda dataset
LPILambdas <- select(LPILambdas, ID, Binomial, Common_name, Class, Red_list_category, Criteria, Percentage, Percentage_based_on, SpeciesSSet, av_lambda, sum_lambda, Length)

#set LPI names to match CalcualtedTrends
colnames(LPILambdas) <- c("ID", "Scientific_Name", "Common_name", "Class", "Red_List_Category", "Red_List_Criteria", "Percentage_incorporated", "Percentage_based_on", "ID_Binomial", "av_lambda", "sum_lambda", "Length")

#Select useful columns only from Calculated Trends dataframe
CalculatedTrends <- select(CalculatedTrends, Scientific_Name, Class, Generation_Length_Years, PercentDecrease10YMin, PercentDecreaseMin, PercentDecrease3GMin, PercentDecrease10YMid, PercentDecreaseMid, PercentDecrease3GMid, PercentDecrease10YMax, PercentDecreaseMax, PercentDecrease3GMax, Lambda10YMin, LambdaMin, Lambda3GMin, Lambda10YMid, LambdaMid, Lambda3GMid, Lambda10YMax, LambdaMax, Lambda3GMax,  ExLambda10YMin, ExLambdaMin, ExLambda3GMin, ExLambda10YMid, ExLambdaMid, ExLambda3GMid, ExLambda10YMax, ExLambdaMax, ExLambda3GMax)

#re-formate CalculatedTrends Scientific_Name to suit LPI dataframe
CalculatedTrends$Scientific_Name <- sub(" ", "_", CalculatedTrends$Scientific_Name)

##merge both dataframes 
##this should give each species ID (population level) in LPI an appropriate calculated species level trends
Trends <- merge(LPILambdas, CalculatedTrends, all = TRUE, by = "Scientific_Name")

#check structure of Trends, ensure class is as a character so they can be merged
str(Trends)
Trends$Class.x <- as.character(Trends$Class.x)
Trends$Class.y <- as.character(Trends$Class.y)
str(Trends)

#merge common names together 
Trends <- Trends%>%mutate(Class = coalesce(Class.y, Class.x))
Trends <- select(Trends, -Class.x, -Class.y)

#save dataframes
write.csv(Trends, "CalculatedLambdas.csv")
write.csv(AvesCalcTrends, "CalcLambdasThreatenedAves.csv")
write.csv(MamsCalcTrends, "CalcLambdasThreatenedMams.csv")          

#This new data set has all species included, those where it was not possible to calculate new IUCN trends for,
#those that we could calculated LPI trends from but are not already in the LPI and those that are already in 
#the LPI with their corresponding calculated trend, or not if it was not possible to calculate it. 

#filter new dataset to just species with either a LPI lambda or a newly calculated lambda
Trends_Either <- filter(Trends, av_lambda | ExLambda10YMin | ExLambdaMin | ExLambda3GMin | ExLambda10YMid | ExLambdaMid | ExLambda3GMid | ExLambda10YMax | ExLambdaMax | ExLambda3GMax != "NA") 

#filter new dataset to just species with both a LPI lambda and a newly calculated lambda
Trends_Both <- filter(Trends, av_lambda & (ExLambda10YMin | ExLambdaMin | ExLambda3GMin | ExLambda10YMid | ExLambdaMid | ExLambda3GMid | ExLambda10YMax | ExLambdaMax | ExLambda3GMax) != "NA") 


###data extraction for results section: means and standard errors for lambdas and declines
##create function for standard error
se <- function(x, na.rm=FALSE) {
if (na.rm) x <- na.omit(x)
sqrt(var(x)/length(x))
}

##average original LPI lambda
mean(LPILambdas$av_lambda, na.rm = TRUE) #-0.00928
se(LPILambdas$av_lambda, na.rm = TRUE) #0.001419
#as an annual percent decrease
#2.1141%
#se = 0.3262

##average IUCN-dervied LPI lambda
#create a new column which is the av LPI lambda or the ExLambdaMid if there is no av LPI lambda
Trends_Either <- Trends_Either%>%mutate(NEWLambda = coalesce(av_lambda, ExLambdaMid))
mean(Trends_Either$NEWLambda) #-0.00938
se(Trends_Either$NEWLambda, na.rm = TRUE) #0.000825
#as an annual percent decrease
#2.1367%
#se = -0.19

#save dataframes
write.csv(Trends_Either, "CalculatedLambdas_Either.csv")
write.csv(Trends_Both, "CalculatedLambdas_Both.csv")


####Pearson's product-moment correlation test####
###FOR ALL VERTEBRATES###
##for our best guess (10 years or 3 gen, whichever is longer) 
##mid
#LPI av_lambda
cor.test(Trends_Both$ExLambdaMid, Trends_Both$av_lambda)
#0.063 95%CL 0.034-0.091

###FOR ALL BIRDS###

#Subset Trends_Both to just bird species
Trends_Both_Aves <- filter(Trends_Both, Class %in% "aves")

##for our best guess (10 years or 3 gen, whichever is longer) 
##mid
#LPI av_lambda
cor.test(Trends_Both_Aves$ExLambdaMid, Trends_Both_Aves$av_lambda)
#0.250 95%CL 0.20-0.30

###FOR ALL MAMMALS###

#Subset Trends_Both to just mammal species
Trends_Both_Mams <- filter(Trends_Both, Class == "mammalia")

##for our best guess (10 years or 3 gen, whichever is longer) 
#LPI av_lambda
cor.test(Trends_Both_Mams$ExLambdaMid, Trends_Both_Mams$av_lambda)
#0.008 95%CL -0.05-0.06


###for all threatened vertebrates### 
Trends_Both_Threatened <- filter(Trends_Both, Red_List_Category %in% c("CR", "VU", "EN"))

#LPI av_lambda
cor.test(Trends_Both_Threatened$ExLambdaMid, Trends_Both_Threatened$av_lambda)
#0.090 95%CL 0.04-0.14


###for only long time-series LPI data### 
##For all vertebrates
#Subset Trends_Both to just Long time-series LPI data species
Trends_Both_Long <- filter(Trends_Both, Length >= 20)

#LPI av_lambda
cor.test(Trends_Both_Long$ExLambdaMid, Trends_Both_Long$av_lambda)
#0.233 95%CL 0.17-0.29


##For birds with long time series
#Subset Trends_Both_Long to just bird species
Trends_Both_Long_Aves <- filter(Trends_Both_Long, Class == "aves")

#LPI av_lambda
cor.test(Trends_Both_Long_Aves$ExLambdaMid, Trends_Both_Long_Aves$av_lambda)
#0.274 95%CL 0.18-0.36


##For mammals with long time series 
#Subset Trends_Both_Long to just mammal species
Trends_Both_Long_Mams <- filter(Trends_Both_Long, Class == "mammalia")

#LPI av_lambda
cor.test(Trends_Both_Long_Mams$ExLambdaMid, Trends_Both_Long_Mams$av_lambda)
#0.127 95%CL 0.0049-0.25

###for only long time-series AND large propotion of global population LPI data### (now using only mid esitmate using 10Y or 3G, whichever longer)
#Subset Trends_Both_Long to just Long time-series AND large proportion of global popualtion LPI data species
Trends_Both_Long_High <- filter(Trends_Both, Length >= 20 & Percentage_incorporated %in% "76-100%")

##For all vertebrates with long time series and a large proporiton 
#LPI av_lambda
cor.test(Trends_Both_Long_High$ExLambdaMid, Trends_Both_Long_High$av_lambda)
#0.589 95%CL 0.31-0.76

##For birds 
#Subset Trends_Both_Long to just Long time-series AND large proportion of global popualtion LPI data species
Trends_Aves_Both_Long_High <- filter(Trends_Both_Long_High, Class %in% "aves")
#LPI av_lambda
cor.test(Trends_Aves_Both_Long_High$ExLambdaMid, Trends_Aves_Both_Long_High$av_lambda)
#0.30 95%CL -0.15-0.65

##For mammals 
#Subset Trends_Both_Long to just Long time-series AND large proportion of global popualtion LPI data species
Trends_Mams_Both_Long_High <- filter(Trends_Both_Long_High, Class %in% "mammalia")
#LPI av_lambda
cor.test(Trends_Mams_Both_Long_High$ExLambdaMid, Trends_Mams_Both_Long_High$av_lambda)
#0.26 95%CL -0.55-0.81

####fit simple linear model for select comparisons#### 

###For all vertebrates - using mid value for 10Yor3G and average LPI lambda
#create model
ModAll <- lm(Trends_Both$ExLambdaMid ~ Trends_Both$av_lambda)
summary(ModAll)
#plot model
plot(Trends_Both$ExLambdaMid ~ Trends_Both$av_lambda)
abline(lm(Trends_Both$ExLambdaMid ~ Trends_Both$av_lambda))


###For all birds - using mid value for 10Yor3G and average LPI lambda
#create model
Modbirds <- lm(Trends_Both_Aves$ExLambdaMid ~ Trends_Both_Aves$av_lambda)
summary(Modbirds)
#plot model
plot(Trends_Both_Aves$ExLambdaMid ~ Trends_Both_Aves$av_lambda)
abline(lm(Trends_Both_Aves$ExLambdaMid ~ Trends_Both_Aves$av_lambda))


###For all mammals - using mid value for 10Yor3G and average LPI lambda
#create model
ModMams <- lm(Trends_Both_Mams$ExLambdaMid ~ Trends_Both_Mams$av_lambda)
summary(ModMams)
#plot model
plot(Trends_Both_Mams$ExLambdaMid ~ Trends_Both_Mams$av_lambda)
abline(lm(Trends_Both_Mams$ExLambdaMid ~ Trends_Both_Mams$av_lambda))


###For all vertebrates with long time series - using mid value for 10Yor3G and average LPI lambda
#create model
ModL <- lm(Trends_Both_Long$ExLambdaMid ~ Trends_Both_Long$av_lambda)
summary(ModL)
#plot model
plot(Trends_Both_Long$ExLambdaMid ~ Trends_Both_Long$av_lambda)
abline(lm(Trends_Both_Long$ExLambdaMid ~ Trends_Both_Long$av_lambda))


###For verts with long time series and a high proportion - using mid value for 10Yor3G and average LPI lambda
##LPI av_lambda
#create model
ModLH <- lm(Trends_Both_Long_High$ExLambdaMid ~ Trends_Both_Long_High$av_lambda)
summary(ModLH)

#plot model
par(mfrow=c(1,1))
par(mar = c(5, 6, 3, 3))
par(mgp.axis=c(4,1,0))
plot(jitter(Trends_Both_Long_High$ExLambdaMid) ~ Trends_Both_Long_High$av_lambda, xlab = "Mean annual LPI species declines (lambdas)", ylab = "Mean annaul IUCN-derived\ndeclines (lambdas)", pch = 1, cex = 0.9, las = 1, bty="l")
abline(lm(Trends_Both_Long_High$ExLambdaMid ~ Trends_Both_Long_High$av_lambda))

#check diagonistics
par(mfrow=c(2,2))
plot(ModLH)

##Q-Q plot not wonderful - can we fix this by including present of GL data as an interation?
##and a column for presence/absence of GL data as logical
Trends_Both_Long_High$GL <- !is.na(Trends_Both_Long_High$Generation_Length_Years)

#form model
ModLHGL <- lm(Trends_Both_Long_High$ExLambdaMid ~ Trends_Both_Long_High$av_lambda * Trends_Both_Long_High$GL)
summary(ModLHGL)
plot(ModLHGL)
#yes it's better like this - however, the graph below that displays this interaction was not presented
#this is because I choose to present corr test results over linear models as the data 
#do not fit linear models very well and corr tests are sufficient to test for the correlation.
#instead the original graph without the interaction was presented. 

#set GL as factor in order to colour the points differently
Trends_Both_Long_High$GL <- as.factor(Trends_Both_Long_High$GL)

##create a colour palette
col.topo <- topo.colors(2)
palette(col.topo)

#plot coparison with two lines - one for calculated with GL and one for calucated without GL
par(mfrow=c(1,1))
par(mgp=c(2,1,0))
plot(jitter(Trends_Both_Long_High$ExLambdaMid) ~ Trends_Both_Long_High$av_lambda, xlab = "Mean annual LPI species declines (lambdas)", ylab = "Mean annaul IUCN-derived\ndeclines (lambdas)", pch = 1, cex = 0.9, col = Trends_Both_Long_High$GL)
abline(lm(Trends_Both_Long_High$ExLambdaMid[Trends_Both_Long_High$GL %in% TRUE] ~ Trends_Both_Long_High$av_lambda[Trends_Both_Long_High$GL %in% TRUE]), col = "#00E5FFFF")
abline(lm(Trends_Both_Long_High$ExLambdaMid[Trends_Both_Long_High$GL %in% FALSE] ~ Trends_Both_Long_High$av_lambda[Trends_Both_Long_High$GL %in% FALSE]), col = "#4C00FFFF")
legend(-0.085, 0, legend = c("With generation length", "Without generation length"), col = c("#00E5FFFF", "#4C00FFFF"), cex=0.6, lty = 1)

#sample size for results section
length(Trends_Both_Long_High)

##For birds with long time series and high proportion - using only Av lambda
#create model
ModLHAves <- lm(Trends_Aves_Both_Long_High$ExLambdaMid ~ Trends_Aves_Both_Long_High$av_lambda)
summary(ModLHAves)
#plot model
plot(jitter(Trends_Aves_Both_Long_High$ExLambdaMid) ~ Trends_Aves_Both_Long_High$av_lambda, xlab = "Mean annual LPI species population declines (lambdas)", ylab = "Calculated annaul species popualation declines (lambdas)", pch = 1, cex = 0.9)
abline(lm(Trends_Aves_Both_Long_High$ExLambdaMid ~ Trends_Aves_Both_Long_High$av_lambda))

##For mammals with long time series and high proportion - using only Av lambda
#create model
ModLHMams <- lm(Trends_Mams_Both_Long_High$ExLambdaMid ~ Trends_Mams_Both_Long_High$av_lambda)
summary(ModLHMams)
#plot model
plot(jitter(Trends_Mams_Both_Long_High$ExLambdaMid) ~ Trends_Mams_Both_Long_High$av_lambda, xlab = "Mean annual LPI species population declines (lambdas)", ylab = "Calculated annaul species popualation declines (lambdas)", pch = 1, cex = 0.9)
abline(lm(Trends_Mams_Both_Long_High$ExLambdaMid ~ Trends_Mams_Both_Long_High$av_lambda))






