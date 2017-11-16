##################################################
# Script for combining data from UN
# Author: Francesca Mancini
# Date created: 2017-11-13
# Date modified: 2017-11-16
##################################################

library(tidyr)
library(readr)
library(reshape2)

# save path to files in an object
dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/UNSDGindicators/"


# create a list with all the files' names of the indicators datasets
files <- list.files(dataFilePath, pattern = "*.csv")

# Create list of data frame names without the ".csv" part
names <-substr(files,1, nchar(files)-4)

# Load all files and assign same name
for(i in names){
  filepath <- file.path(dataFilePath, paste(i,".csv",sep=""))
  assign(i, read.csv(filepath, header = T, stringsAsFactors = F, na.strings=c("","NA")))
}

# put datasets into a list of dataframes
UN_indicators.all <- lapply(ls(pattern="SDG_Indicators"), function(x) get(x))

# transform each dataset into a long format with a year column
UN_indicators.all <- lapply(UN_indicators.all, 
                            function(x) gather(x, key = "year", value = "value", 
                                               select = which(grepl("X",names(x))==TRUE)))

# only keep mean values and discard upper and lower bounds
UN_indicators.all <- lapply(UN_indicators.all, 
                            function(x) subset(x, x$Value.type=="" | is.na(x$Value.type), 
                                               select = -c(which(grepl("FN", names(x))==TRUE)))) 
                            
# put all the dataframes together
UN_indicators_combined <- do.call(rbind, UN_indicators.all)

# assign the right format to variables
UN_indicators_combined$year <- parse_number(UN_indicators_combined$year)

for(i in 1:22){
  UN_indicators_combined[,i] <- as.factor(UN_indicators_combined[,i])
}

UN_indicators_combined$value <- as.numeric(UN_indicators_combined$value)



# transform into wide format with every indicators having its own column
# some indicators are disaggregated by sex and age groups
# use dcast to transform dataset into wide format 
# every indicator has its own column also split by age or sex
UN_indicators.split <- dcast(UN_indicators_combined, 
                             Country.or.Area.Name + year ~ Series.Code + Age.group + Sex, 
                             value.var = "value")
# only keep data between 2004 and 2015
UN_indicators.split <- subset(UN_indicators.split, year > 2003 & year < 2015)
# save dataset
write.csv(UN_indicators.split, paste(dataFilePath, "UN_indicators_combined.csv", sep = ""), row.names = F)

# create a legend for every indicator series
UN_indicators.legend <- lapply(UN_indicators.all, function(x) 
  unique(x[c("Series.Code", "Series.Description", "Indicator.Ref", "Sex", "Age.group")]))

UN_indicators.legend <- do.call(rbind, UN_indicators.legend)
write.csv(UN_indicators.legend, paste(dataFilePath, "UN_indicators_legend.csv", sep = ""), row.names = F)
