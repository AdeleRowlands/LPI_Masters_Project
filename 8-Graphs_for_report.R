####Script 8####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Graphs for report###

##start next stage by clearing environment 
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
require(ggpubr)

#set working direcotry (left blank for reader to include their own directory).
setwd()

#check working director
getwd()
dir()

#import useful datasets
data <- read.csv("CalculatedLambdas.csv", header = TRUE, row.names = 1)
ThreatAves <- read.csv("CalcLambdasThreatenedAves.csv", header = TRUE, row.names = 1)
ThreatMams <- read.csv("CalcLambdasThreatenedMams.csv", header = TRUE, row.names = 1)
OurLambdas <- read.csv("IUCN-derived-Lambdas&Declines.csv", header = TRUE, row.names = 1)
data$Vertebrate <- "Vertebrate"
OurLambdas$Vertebrate <- "All vertebrates"

###VIOLIN PLOTS - PRESENTED- using percent decline, medium estimate and 3G or 10Y method 
##for all vertebrates
vert <- ggplot(OurLambdas, aes(x = Vertebrate, y = PercentDecreaseMid)) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)")  + geom_violin(trim = TRUE) + geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1)+ ylim(-2, 20.5)+ theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

##for threatened vertebrates
dataThreat <- filter(OurLambdas, IUCN_Category %in% c("CR", "EN", "VU"))
dataThreat$Vertebrate <- "Threatened vertebrates"
Tvert <- ggplot(dataThreat, aes(x = Vertebrate, y = PercentDecreaseMid)) + geom_violin(trim = TRUE) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)") + geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1) + ylim(-2, 20.5) + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

##for all birds
AllAvesData <- filter(OurLambdas, Class == "aves")
AllAvesData$Lable <- "All birds"
bird <- ggplot(AllAvesData, aes(x = Lable, y = PercentDecreaseMid)) + geom_violin(trim = TRUE) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)") + geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1)+ ylim(-2, 20.5)+ theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

#for threatened birds
ThreatAves$Lable <- "Threatened birds"
Tbird <- ggplot(ThreatAves, aes(x = Lable, y = PercentDecreaseMid)) + geom_violin(trim = TRUE) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)") + geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1) + ylim(-2, 20.5) + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

##for all mammals
AllMamsData <- filter(OurLambdas, Class == "mammalia")
AllMamsData$Lable = "All mammals"
mam <- ggplot(AllMamsData, aes(x = Lable, y = PercentDecreaseMid)) + geom_violin(trim = TRUE) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)") + geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1) + ylim(-2, 20.5) + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

##for threatened mammals
ThreatMams$Lable <- "Threatened mammals"
Tmam <- ggplot(ThreatMams, aes(x = Lable, y = PercentDecreaseMid)) + geom_violin(trim = TRUE) + labs(x = " ", y = "Cumulative percent population\ndecline per annum (%)")+ geom_boxplot(width = 0.1) + stat_summary(fun = mean, geom = "crossbar", colour = "red", width=0.1) + ylim(-2, 20.5) + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

##arrange as pannels
#use the muliplot function - available online at 'Cookbook for R: Mulitple graphs on one page'
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#use mulitplot function
multiplot(vert, Tvert, bird, Tbird, mam, Tmam, cols = 3)

###VIOLIN PLOTS - using ExLambdas, medium estimate and 3G or 10Y method - not presented

##for all vertebrates
ggplot(data, aes(x = Vertebrate, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "All Vertebrates", y = "Lambda")

##for threatened vertebrates
ggplot(dataThreat, aes(x = Vertebrate, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "Threatened Vertebrates", y = "Lambda")

##for all birds
ggplot(AllAvesData, aes(x = Class, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "All Birds", y = "Lambda")

#for threatened birds
ggplot(ThreatAves, aes(x = Class, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "Threatened Birds", y = "Lambda")

##for all mammals
ggplot(AllMamsData, aes(x = Class, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "All Mammals", y = "Lambda")

##for threatened mammals
ggplot(ThreatMams, aes(x = Class, y = ExLambdaMid)) + geom_violin(trim = TRUE) + stat_summary(fun = median, geom = "point", shape = 23) + geom_boxplot(width = 0.1) + labs(x = "Threatened Mammals", y = "Lambda")


##LPI and calculated trends comparison plot - Presented

#read in dataframe with Y, Ymin and Ymax for each year, made in excel by extrapolating the min, mid and max trend over 45 years. 
plot <- read.csv("LambdasForTrendGraph.csv", header = TRUE, row.names = 1)
#sort column names
colnames(plot) <- c("LPI_final", "CI_low", "CI_high")

#read in data to make species-level LPI plot 
poplpi <- read.table("2018_global_infile_Results.txt", header = TRUE) 
poplpi$years <- seq(from = 1970, to = 2014, by = 1)
colnames(poplpi) <- c("lpi", "CI_low", "CI_high", "years")

#read in data to make population-level LPI plot 
sp_lpi <- read.csv("sp_level_lpi.csv", header = TRUE)
colnames(sp_lpi) <- c("years", "lpi", "CI_low", "CI_high")

#remove last two rows of plot and sp_lpi so they have 45 obs like lpi
sp_lpi <-sp_lpi[-c(46, 47), ]
plot <-plot[-46, ]

#make LPI plot
ggplot_lpi(plot,xlims=c(1970, 2015), ylims = c(0,1.4), col = "light grey") + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() +
 geom_ribbon(data = poplpi, aes(ymin = CI_low, ymax = CI_high), fill = "Blue", alpha = 0.5) +
  geom_ribbon(data = sp_lpi, aes(ymin = CI_low, ymax = CI_high), fill = "mediumpurple1", alpha = 0.5) + geom_line(aes(y=poplpi$lpi), colour = "blue") + geom_line(aes(y=plot$LPI_final), colour = "Red")+ geom_line(aes(y=sp_lpi$lpi), colour = "Purple") + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14))


                                                                                                                                                             