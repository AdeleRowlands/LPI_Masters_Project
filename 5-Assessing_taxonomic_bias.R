####Script 5####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Comparison of LPI species representation, pre and post inclusion of LPI species###

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

###To compare pre and post inclusion of Red List species I will be conducting a binomial test for proportions between full LPI dataset and full Red List dataset.
#Then I will be conducting a binomial test between the IUCN-derived full dataset (trends either from original LPI or calculated using our method - Trends_Either)
#then the results of these two binomials will be compared. 

###import useful datasets
dir()
Trends_Either <- read.csv("CalculatedLambdas_Either.csv", header = TRUE, row.names = 1)
LPI <- read.csv("Full_LPI_Species_Dataset.csv", header = TRUE, row.names = 1)
RLdata <- read.csv("RedListDataframe.csv", header = TRUE)

##tidy RLdata

#select useful columns
RLdata <- select(RLdata, scientific_name, class_name, category)
colnames(RLdata) <- c("Scientific_Name", "Class", "IUCN_Category")

#replace class to lower case
RLdata$Class <- tolower(RLdata$Class)

#filter to just vertebrate species
RLdata <- filter(RLdata, Class %in% c("actinopterygii", "amphibia", "cephalaspidomorphi", "chondrichthyes", "mammalia", "reptilia", "sarcopterygii", "aves", "elasmobranchii", "holocephali", "myxini", "sarcopterygii"))

#change class to a factor
str(RLdata)
RLdata$Class <- as.factor(RLdata$Class)
str(RLdata)

##Tidy Trends_Either
#remove duplicates of species due to use of population in LPI - we are only looking to see if we have expanded coverage of species
Trends_Either <- Trends_Either[!duplicated(Trends_Either[,'Scientific_Name']),]

#change Trends_Either class to lower case
Trends_Either$Class <- tolower(Trends_Either$Class)


#Calculate proportion of each class for each of the three datasets

##No. birds
#LPI
sum(LPI$Class %in% "aves") #1579 31.97%
#IUCN-Derived LPI
sum(Trends_Either$Class %in% "aves") #5208 32.98%
#Red List
sum(RLdata$Class %in% "aves") #11147 32.57%

##No. mammals
#LPI
sum(LPI$Class %in% "mammalia") #674 13.65%
#All
sum(Trends_Either$Class %in% "mammalia") #2168 13.73%
#Red List
sum(RLdata$Class %in% "mammalia") #6295 13.91%

##No. fish
#LPI
sum(LPI$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")) #2137 43.27%
#All
sum(Trends_Either$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")) #3933 24.91%
#Red List
sum(RLdata$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")) #20567 39.01%

##No. amphibians
#LPI
sum(LPI$Class %in% "amphibia") #316 6.40%
#All
sum(Trends_Either$Class %in% "amphibia") #1885 11.94%
#Red List
sum(RLdata$Class %in% "amphibia") #6825 12.95%

##No. reptiles
#LPI
sum(LPI$Class %in% "reptilia") #233 4.72%
#All
sum(Trends_Either$Class %in% "reptilia") #2404 15.22%
#Red List
sum(RLdata$Class %in% "reptilia") #7885 14.96%



###Binomial test of proportions between orginial LPI and Red List###

##For birds
prop.test(c(sum(LPI$Class %in% "aves"), nrow(LPI)), c(sum(RLdata$Class %in% "aves"), nrow(RLdata)), correct = FALSE)

##For mammals
prop.test(c(sum(LPI$Class %in% "mammalia"), nrow(LPI)), c(sum(RLdata$Class %in% "mammalia"), nrow(RLdata)), correct = FALSE)

##For fish
prop.test(c(sum(LPI$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")), nrow(LPI)), c(sum(RLdata$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")), nrow(RLdata)), correct = FALSE)

##For amphibians
prop.test(c(sum(LPI$Class %in% "amphibia"), nrow(LPI)), c(sum(RLdata$Class %in% "amphibia"), nrow(RLdata)), correct = FALSE)

##For reptiles
prop.test(c(sum(LPI$Class %in% "reptilia"), nrow(LPI)), c(sum(RLdata$Class %in% "reptilia"), nrow(RLdata)), correct = FALSE)


###Binomial test of proportions between IUCN-dervied LPI and Red List###

##For birds
prop.test(c(sum(Trends_Either$Class %in% "aves"), nrow(Trends_Either)), c(sum(RLdata$Class %in% "aves"), nrow(RLdata)), correct = FALSE)

##For mammals
prop.test(c(sum(Trends_Either$Class %in% "mammalia"), nrow(Trends_Either)), c(sum(RLdata$Class %in% "mammalia"), nrow(RLdata)), correct = FALSE)

##For fish
prop.test(c(sum(Trends_Either$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")), nrow(Trends_Either)), c(sum(RLdata$Class %in% c("actinopterygii", "cephalaspidomorphi", "chondrichthyes", "myxini", "sarcopterygii")), nrow(RLdata)), correct = FALSE)

##For amphibians
prop.test(c(sum(Trends_Either$Class %in% "amphibia"), nrow(Trends_Either)), c(sum(RLdata$Class %in% "amphibia"), nrow(RLdata)), correct = FALSE)

##For reptiles
prop.test(c(sum(Trends_Either$Class %in% "reptilia"), nrow(Trends_Either)), c(sum(RLdata$Class %in% "reptilia"), nrow(RLdata)), correct = FALSE)
