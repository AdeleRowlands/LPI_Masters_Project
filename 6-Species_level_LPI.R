####Script 6####

###02/2020 - 08/2020 - ANR
###Module: Research Project
###Estimating global abundance trends from species assessments
###Creating a species level LPI - code initally written by Robin Freeman and edited by me###

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

# Load IUCN-dervied estimated lambdas
iucn_trends = read.csv("IUCN-derived-Lambdas&Declines.csv")

# Subset to only those species with lambdas
iucn_trends_with_lambdas = subset(iucn_trends, !is.na(LambdaMid))

# How many species with lambdas calcualted
nrow(iucn_trends_with_lambdas)

# How many are in the LPI?
iucn_trends_with_lambdas_inlpi = subset(iucn_trends_with_lambdas, PresentInLPI == TRUE)
nrow(iucn_trends_with_lambdas_inlpi)

# How many aren't in the LPI?
iucn_trends_with_lambdas_notinlpi = subset(iucn_trends_with_lambdas, PresentInLPI == FALSE)
nrow(iucn_trends_with_lambdas_notinlpi)


## Create an LPI trend for species already in the LPI

# For each species, generate a time series starting at 1 and increasing by the lambda each year from 1970 to 2014

years = 1970:2014
poplist = list()

for (idx in 1:nrow(iucn_trends_with_lambdas_inlpi)) {
  this_species = iucn_trends_with_lambdas_inlpi[idx, ]
  
  # So lambdas are 0 for first year, then calculated species lambda for each additional year
  lambdas = c(0, rep(this_species$ExLambdaMid, (length(years)-1)))
  
  # Time series is the cumulative product (repeated multiplication of the exponentiated lambdas)
  timeseries = cumprod(exp(lambdas))
  
  # Make a data frame
  dat = data.frame(Binomial = gsub(" ", "_", this_species$Scientific_Name), ID = idx, year = years, popvalue = timeseries)

  # Put in list
  poplist[[idx]] = dat
}

# Turn list into big dataframe
poplist_df = do.call(rbind, poplist)

# Write to file (As a tab separated txt for rlpi)
write.table(poplist_df, "iucn_in_lpi_pops.txt", sep="\t", row.names=FALSE, quote = F)


# popdata is now in 'iucn_in_lpi_pops.txt' so we can now create an infile to point to that data for rlpi
filename = "iucn_in_lpi_pops.txt"
# It doesn't matter as theres only 1 pop file, but the infile has 1s for weightings and group to ensure 
# no regional or taxonomic weightings
in_file_data <- data.frame(FileName=filename, Group=1, Weighting=1)
in_lpi_infile_name = gsub("pops.txt", "infile.txt", filename)
write.table(in_file_data, in_lpi_infile_name, sep="\t", row.names=FALSE)

# Run LPI Main on IUCN generated data...

library(rlpi)
iucn_in_lpi <- LPIMain(in_lpi_infile_name, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100)
write.csv(iucn_in_lpi, "iucn_in_lpi.csv")

#too avoid running long code just read in the saved dataframe to make graph 
iucn_in_lpi <- read.csv("iucn_in_lpi.csv", row.names = 1)
ggplot_lpi(iucn_in_lpi, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2011)) + labs(x = "Years", y = "Index value (1970 = 1)") 



## repeat this for species that are not in LPI

# For each species, generate a time series starting at 1 and increasing by the lambda each year from 1970 to 2014

years = 1970:2014
poplist = list()

for (idx in 1:nrow(iucn_trends_with_lambdas_notinlpi)) {
  this_species = iucn_trends_with_lambdas_notinlpi[idx, ]
  
  # So lambdas are 0 for first year, then calculated species lambda for each additional year
  lambdas = c(0, rep(this_species$ExLambdaMid, (length(years)-1)))
  
  # Time series is the cumulative product (repeated multiplication of the exponentiated lambdas)
  timeseries = cumprod(exp(lambdas))
  
  # Make a data frame
  dat = data.frame(Binomial = gsub(" ", "_", this_species$Scientific_Name), ID = idx, year = years, popvalue = timeseries)
  
  # Put in list
  poplist[[idx]] = dat
}

# Turn list into big dataframe
poplist_df = do.call(rbind, poplist)

# Write to file (As a tab separated txt for rlpi)
write.table(poplist_df, "iucn_not_in_lpi_pops.txt", sep="\t", row.names=FALSE, quote = F)


# so popdata is now in 'iucn_not_in_lpi_pops.txt' we can now creae an infile to point to that data for rlpi
filename = "adele_iucn_not_in_lpi_pops.txt"
# It doesn't matter as theres only 1 pop file, but the infile has 1s for weightings and group to ensure 
# no regional or taxonomic weightings
in_file_data <- data.frame(FileName=filename, Group=1, Weighting=1)
notinlpi_infile_name = gsub("pops.txt", "infile.txt", filename)
write.table(in_file_data, notinlpi_infile_name, sep="\t", row.names=FALSE)

# Run LPI Main on IUCN generated data...

library(rlpi)
iucn_not_in_lpi <- LPIMain(notinlpi_infile_name, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100)
write.csv(iucn_not_in_lpi, "iucn_not_in_lpi.csv")

#to avoid running long code mulitple times just read in the saved dataframe to make graph 
iucn_not_in_lpi <- read.csv("iucn_not_in_lpi.csv", row.names = 1)
ggplot_lpi(iucn_not_in_lpi, ylims = c(0.3,1.05), xlims = c(1970, 2011), line_col = "turquoise") 

##The file "Db_output_20191205_final.csv" is confidential and therefore could not be submitted as
##part of my MSc project - therefore all subsiquent code in this script cannot be run by markers. 
##To show that this code generates the dataframes needed for the figures displayed in the report
##the dataframes produced by this code have been submitted with the report and can be read in at the
##appropriate places indicated below to produce the graphs. 

##create a species level LPI for current LPI species 
# Load LPI data and create species level LPI (no regional or taxonomic weightings...)
lpi_data <- read.csv("Db_output_20191205_final.csv", na.strings = 'NULL')
LPR2020_pops <- lpi_data$Replicate == '0'  ## To exclude the replicates for global level/Classes

# This would create fake species for a 'population level' LPI
#lpi_data$Binomial = paste(lpi_data$ID, lpi_data$Binomial, sep="_")

sp_level_index <- create_infile(lpi_data, index_vector=LPR2020_pops, name="global_sp_level_2020")
sp_level_lpi <- LPIMain(sp_level_index, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100)
write.csv(sp_level_lpi, "sp_level_lpi.csv")

#as the file "Db_output_20191205_final.csv" is confidential, read in the saved dataframe to make graph 
sp_level_lpi <- read.csv("sp_level_lpi.csv", row.names = 1)
ggplot_lpi(sp_level_lpi, ylims = c(0.3,1.05), line_col = "turquoise")

##plot weighted LPI for comparision
## Load and plot latest LPI results (cropped to 2014)
dat_global_2018= read.table("2018_global_infile_Results.txt")
ggplot_lpi(dat_global_2018, ylims = c(0.3,1.05), line_col = "turquoise")


##create combined species level LPI
# Add data for IUCN species missing from LPI to observe impact on trend
# as "sp_level_index" is derived from "Db_output_20191205_final.csv" - it also cannot be read it
# therefore, the next area of code that can be run is from line 211
sp_level_infile = sp_level_index
# Get species level popfile
infile_dat = read.table(sp_level_index, header = T, as.is=T)
sp_level_popfile_name = infile_dat$FileName

# Concatenate IUCN popdata to end of file
lpi_pops = read.table(sp_level_popfile_name, header = T, as.is=T)
iucn_pops = read.table("iucn_not_in_lpi_pops.txt", header = T, as.is=T)

# Need to add a large number to all IDs in IUCN pops so they don't clash
iucn_pops$ID = iucn_pops$ID + 50000
# Add all pop trends to one database 
combined_pops = rbind(lpi_pops, iucn_pops)

write.table(combined_pops, "combined_pops.txt", sep="\t", row.names=FALSE, quote = F)


# so popdata is now in 'combined_pops.txt' we can now creae an infile to point to that data for rlpi
filename = "combined_pops.txt"
# It doesn't matter as theres only 1 pop file, but the infile has 1s for weightings and group to ensure 
# no regional or taxonomic weightings
in_file_data <- data.frame(FileName=filename, Group=1, Weighting=1)
combined_infile_name = gsub("pops.txt", "infile.txt", filename)
write.table(in_file_data, combined_infile_name, sep="\t", row.names=FALSE)

## Run LPI Main on combined species level data...
combined_lpi <- LPIMain(combined_infile_name, PLOT_MAX = 2015, BOOT_STRAP_SIZE = 100)
write.csv(combined_lpi, "combined_lpi.csv")

##as the file "Db_output_20191205_final.csv" is confidential, read in the saved dataframe to make graph 
combined_lpi <- read.csv("combined_lpi.csv", row.names = 1)
ggplot_lpi(combined_lpi, ylims = c(0.3, 1.1), line_col = "turquoise")

###graphs to be presented

##first re-format sp_level_lpi as sp_level_lpi2 in order for it to be added as a background comparision trend in Fig. 6c, d and e
sp_level_lpi2 <- sp_level_lpi
sp_level_lpi2$years <- seq(from = 1970, to = 2016, by = 1)
colnames(sp_level_lpi2) <- c("lpi", "CI_low", "CI_high", "years")

##now plot each graph
ggplot_lpi(iucn_in_lpi, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2011)) + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) +
  geom_ribbon(data = sp_level_lpi2, aes(ymin = CI_low, ymax = CI_high), fill = "grey", alpha = 0.5) + geom_line(aes(y=sp_level_lpi2$lpi), col = "Black") 

ggplot_lpi(iucn_not_in_lpi, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2011)) + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) +      
  geom_ribbon(data = sp_level_lpi2, aes(ymin = CI_low, ymax = CI_high), fill = "grey", alpha = 0.5) + geom_line(aes(y=sp_level_lpi2$lpi), col = "Black")
  
ggplot_lpi(combined_lpi, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2015)) + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) +
  geom_ribbon(data = sp_level_lpi2, aes(ymin = CI_low, ymax = CI_high), fill = "grey", alpha = 0.5) + geom_line(aes(y=sp_level_lpi2$lpi), col = "Black")

ggplot_lpi(sp_level_lpi, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2015)) + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 
                                                                                                                                                                                                                     
ggplot_lpi(dat_global_2018, ylims = c(0.3,1.05), line_col = "turquoise",xlims = c(1970, 2015)) + labs(x = "Years", y = "Index value (1970 = 1)") + theme_classic() + theme(axis.text = element_text(size = 14)) + theme(axis.title = element_text(size = 14)) 

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
multiplot(po,not_in, com, sp, in_lpi, cols = 2)


###option of having all plots on one graph - not presented
combined_lpi <- read.csv("combined_lpi.csv")
iucn_in_lpi <- read.csv("iucn_in_lpi.csv")
sp_level_lpi <- read.csv("sp_level_lpi.csv")
iucn_not_in_lpi <- read.csv("iucn_not_in_lpi.csv")

colnames(combined_lpi) <- c("years", "lpi", "CI_low", "CI_high")
colnames(iucn_in_lpi) <- c("years", "lpi", "CI_low", "CI_high")
colnames(sp_level_lpi) <- c("years", "lpi", "CI_low", "CI_high")
colnames(iucn_not_in_lpi) <- c("years", "lpi", "CI_low", "CI_high")

#remove last two rows of each so all have 45 obs like dat_global_2018
combined_lpi <-combined_lpi[-c(46, 47), ]
iucn_in_lpi <-iucn_in_lpi[-c(46, 47), ]
sp_level_lpi <-sp_level_lpi[-c(46, 47), ]
iucn_not_in_lpi <-iucn_not_in_lpi[-c(46, 47), ]

ggplot_lpi(dat_global_2018, ylims = c(0.3,1.05), line_col = "black",xlims = c(1970, 2011)) + 
  geom_ribbon(data = combined_lpi, aes(ymin = CI_low, ymax = CI_high), fill = "paleturquoise2") +
  geom_ribbon(data = sp_level_lpi, aes(ymin = CI_low, ymax = CI_high), fill = "mediumpurple1") + 
  geom_ribbon(data = iucn_not_in_lpi, aes(ymin = CI_low, ymax = CI_high), fill = "red") + 
  geom_ribbon(data = iucn_in_lpi, aes(ymin = CI_low, ymax = CI_high), fill = "orchid1") + geom_line(aes(y=combined_lpi$lpi)) + geom_line(aes(y=iucn_in_lpi$lpi)) + geom_line(aes(y=sp_level_lpi$lpi)) + geom_line(aes(y=iucn_not_in_lpi$lpi))
  
