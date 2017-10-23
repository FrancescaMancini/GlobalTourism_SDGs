#################################################
#Script for aggregating filtered flickr  
#pictures by country and year
#Author: Francesca Mancini
#Date created: 2017-10-23
#Date modified: 2017-10-23
#################################################

library(data.table)
library(sp)
library(rworldmap)
library(lubridate)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)

summary(world)

# load flickr filtered dataset
flickr <- fread("C:\\Users\\r03fm14\\OneDrive - University of Aberdeen\\Data\\Flickr100M\\filteredData.txt",
                colClasses = c("character", "character", "character", "numeric", "numeric"), sep = " ", header = T)

# make it spatial
coordinates(flickr) <- c("longitude", "latitude")
# assign same projection as world map
proj4string(flickr) <- proj4string(world)
# plot on the map
plot(flickr, pch = 20, col = "steelblue")
plot(world, add = T)

##################################################
# this code was run on the University of Aberdeen 
# High Performance Computer Cluster Maxwell
# on a single node with 32G memory
##################################################

# overlay points over polygons 
# (which country is every picture from?)
flickr_country <- over(flickr, world)
# add a column in the original dataframe 
# to store the country
flickr <- cbind(flickr@data, flickr_country$ADMIN)
# change column name
names(flickr)[4] <- "Country"

##################################################

flickr <- fread("C:\\Users\\r03fm14\\OneDrive - University of Aberdeen\\Data\\Flickr100M\\filteredData.txt",
                colClasses = c("character", "character", "character", "character"), sep = " ", header = T)

flickr$year<-year(ymd_hms(flickr$date))             #create variable year

# some observations do not have a date
# delete rows where date is null

flickr <- flickr[-which(flickr$date=="null"),]

summary(flickr$year)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1    2008    2010    2010    2012    8981 

# some of the dates were misreported
# delete all those later than 2014
flickr <- flickr[-which(flickr$year>2014),]

# and all those earlier than 2004 (Flickr launch year)
flickr <- flickr[-which(flickr$year<2004),]

# divide date from time
flickr$dateonly<-as.character(sapply(strsplit(flickr$date, "_"), "[[", 1))   
# find multiple pictures from the same photographer in the same day and in the same country
d<-duplicated(flickr[,c(2,3,4)])                        
# eliminate duplicates
flickr_VD<-flickr[-which(d=="TRUE"),]               


