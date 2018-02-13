###################################################
# Script for analysis of governance
# indicators and tourism
# Author: Francesca Mancini
# Date created: 2018-02-12
# Date modified: 2018-02-13 
##################################################
library(tidyr)
library(readr)

# save data directory as an object
dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/WB_governance/"

# create a list with all the files' names of the governance indicators
files <- list.files(dataFilePath, ".txt")

# create a function to read in the dataset, tidy it and apply to all data files
governance <- lapply(files, function(x){
  # read each dataset in
  # skip 14 rows of title and legend
  # fill = T takes care of empty rows
  # need to specify NA string, but due to the "#" symbol we need to to turn off the interpretation 
  # of comments with comment.char otherwise it will interpret every column that starts with NA string as empty
  # we also skip the first column because it contains country names with special characters
  # quote = "" disables quoting characters
  dat <- read.table(paste(dataFilePath, x, sep = ""), skip = 14, na.strings = "#N/A", 
                    header = T, fill = T, sep = "\t", comment.char = "",
                    colClasses = c("NULL", "character", rep("numeric", 108)), quote = "")
  
  # only keep country codes and columns that start with "Estimate"
  dat <- dat[, c(1, grep("^Estimate", names(dat)))] 

  # rename columns
  names(dat) <- c("ISO3","1996", "1998", "2000", "2002", "2003", "2004", 
                  "2005", "2006", "2007", "2008", "2009", "2010", 
                  "2011", "2012", "2013", "2014", "2015", "2016")
  
  # make country code a factor
  dat$ISO3 <- as.factor(dat$ISO3)
  
  # transform to long format with one row per year
  dat <- gather(dat, key = "year", value = "value", 2:19)
  
  # variable "value" name is file name without .txt extension
  names(dat)[3] <- strsplit(x, "\\.")[[1]][1]
  
  # make year a number
  dat$year <- parse_number(dat$year)
  
  # only keep data between 2004 and 2014
  dat <- dat[-which(dat$year<2004 | dat$year>2014),]

  return(dat) 
})

# merge all datafrmaes in the list into one dataframe containing all indicators
governance <- Reduce(function(x,y) merge(x, y, by= c("ISO3", "year")), governance)
