##################################################
# Script for combining data from
# UNWTO, WB and Flickr 
# Author: Francesca Mancini
# Date created: 2017-10-27
# Date modified: 
##################################################

dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/"
indicators <- read.table(paste(dataFilePath, "WBSDGindicators/SDG_IndicatorsData.txt", sep = ""), 
                         header = T, colClasses = c("character", "character", "character", "character",
                                                    rep("numeric", 28)), na.string = "..", sep = "\t")


# domestic_guests <- read.table(paste(dataFilePath, "UNWTO/Domestic_Accommodation_Guests.txt", sep = ""), 
#                               header = T, colClasses = c("character", "character",rep("numeric", 21)), 
#                               na.string = "..", sep = "\t")
