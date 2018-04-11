###################################################
# Script for calculating proportion
# of nature flickr pictures inside 
# Protected Areas
# Author: Francesca Mancini
# Date created: 2018-02-12
# Date modified: 2018-4-11 
##################################################

#####################################################
# this code was run on the University of Aberdeen 
# High Performance Computer Cluster Maxwell
# on a single node with 64G memory
#####################################################


library(data.table)
library(sp)
library(rworldmap)
library(lubridate)
library(rgdal)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")

flickrdataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/Flickr100M/"

# read the data
flickr_nat <- fread(paste(flickrdataFilePath, "filteredData_sp.txt", sep = ""), header = T, sep = " ", 
                    colClasses=c("character", "character","character", "numeric","numeric"))

flickr_nat_sp <- flickr_nat

# make it spatial
coordinates(flickr_nat_sp) <- c("longitude", "latitude")
# assign same projection as world map
proj4string(flickr_nat_sp) <- proj4string(world)
# plot on the map
plot(flickr_nat_sp, pch = 20, col = "steelblue")
plot(world, add = T)

# overlay points over polygons 
# (which country is every picture from?)
flickr_nat_country <- over(flickr_nat_sp, world)
# add additional columns in the original dataframe 
# to store the country and long/lat
flickr_nat <- cbind(flickr_nat_sp@data, flickr_nat_country$ADMIN, flickr_nat$longitude, flickr_nat$latitude)
# change column name
names(flickr_nat)[4:6] <- c("Country", "longitude", "latitude")

# delete photos taken before 2004 and after 2014
# create variable year
flickr_nat$year<-year(ymd_hms(flickr_nat$date))             

# some observations do not have a date
# delete rows where date is null

flickr_nat <- flickr_nat[-which(flickr_nat$date=="null"),]


flickr_nat <- flickr_nat[-which(flickr_nat$year>2014),]

# and all those earlier than 2004 (Flickr launch year)
flickr_nat <- flickr_nat[-which(flickr_nat$year<2004),]

# # divide date from time
# flickr_nat$dateonly <- as.character(sapply(strsplit(flickr_nat$date, " "), "[[", 1))   
# # find multiple pictures from the same photographer in the same day and in the same country
# d_nat <- duplicated(flickr_nat[,c(2, 4, 6)])                        
# # eliminate duplicates
# flickr_nat_VD <- flickr_nat[-which(d_nat=="TRUE"),]               

# delete photos at sea
nat_terr <- flickr_nat[-which(is.na(flickr_nat$Country)==TRUE),]

# # count nature visitor days per country
# count_NVD <- data.frame(table(nat_VD_terr$Country))
# # give country column the same name as in the world dataset
# names(count_NVD)[1]<- "ADMIN"
# # save
# write.table(count_NVD, "./Flickr/NFVD_agg.txt",
#             row.names = F, sep = "\t")
# 
# 
# # count nature visitor days per country per year
# count_NVD_year <- data.frame(table(nat_VD_terr$Country, nat_VD_terr$year))
# 
# names(count_NVD_year) <- c("country", "year", "FUD")
# 
# write.table(count_NVD_year, "./Flickr/NFVD_year.txt",
#             row.names = F, sep = "\t")

library(data.table)
library(sp)
library(fastshp)
library(rgdal)


flickr_nat <- fread(paste(flickrdataFilePath, "filteredData_sp_country.txt", sep = ""), header = T, sep = " ", 
                    colClasses=c("character", "character","character", "character", "numeric","numeric", "numeric"))

# read in the global PA dataset with fastshp
PA <- read.shp("WDPA_Feb2018-shapefile-polygons.shp", format = "polygon")

# duplicate dataset
flickr_terr <- flickr_nat

# year by year go through each point and determine which PA it is in
flickr_dat_PA <- lapply(unique(flickr_terr$year), function(x){            # apply the custom function to all the subsets of the points by year
  
  print(paste("working on year", x, sep = " "))                           # print a message to track which year the function is working on
  
  flickr_dat <- flickr_terr[flickr_terr$year == x,]                       # subset the points dataset
  
  inside(PA, flickr_dat$longitude, flickr_dat$latitude)                   # spatial join
})


#save(flickr_dat_PA, file = "Results_PA.RData")                            # save the list object


# now we have a list of 11 elements (11 years)
# each element is a vector of the same length as the number of points in that year
# the value is either NA (not in any polygon) or a number (the index of the matching polygon)
# we need to get the other attributes from the points and the year of designation from the polygons

PA_dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/Protected Areas/"

PA <- readOGR(dsn = paste(PA_dataFilePath,"WDPA_Feb2018-shapefile-polygons.shp", sep = ""), layer = "WDPA_Feb2018-shapefile-polygons")

for(y in 1:length(flickr_dat_PA)){
  years <- unique(flickr_nat$year)[y]
  assign(paste("flickr_PA", years, sep = "_"), data.frame(polygon_index = flickr_dat_PA[[y]],
                                                         photoID = flickr_nat[flickr_nat$year == years, "photoID"],
                                                         userID = flickr_nat[flickr_nat$year == years, "userID"],
                                                         date = flickr_nat[flickr_nat$year == years, "date"],
                                                         country = flickr_nat[flickr_nat$year == years, "Country"],
                                                         longitude = flickr_nat[flickr_nat$year == years, "longitude"],
                                                         latitude = flickr_nat[flickr_nat$year == years, "latitude"],
                                                         year = flickr_nat[flickr_nat$year == years, "year"]))
}

flickr_PA <- rbind(flickr_PA_2004, flickr_PA_2005, flickr_PA_2006,
                   flickr_PA_2007, flickr_PA_2008, flickr_PA_2009,
                   flickr_PA_2010, flickr_PA_2011, flickr_PA_2012,
                   flickr_PA_2013, flickr_PA_2014)

flickr_PA$PA_year <- rep(NA,dim(flickr_PA)[1])

flickr_PA$PA_year <- PA@data[flickr_PA$polygon_index, "STATUS_YR"]

##############################################################
############### end of R code run in Maxwell #################
##############################################################


#write.table(flickr_PA, "Flickr_PA.txt", row.names = F, sep = "\t")
#flickr_dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/Flickr100M/"
#flickr_PA <- fread(paste(flickr_dataFilePath, "Flickr_PA.txt", sep = ""), header = T, stringsAsFactors = F,
#                   colClasses=c("numeric", "character","character", "character", "character", "numeric","numeric", "numeric", "numeric"))

flickr_PA$PA <- ifelse(is.na(flickr_PA$polygon_index) != T & flickr_PA$year >= flickr_PA$PA_year, 1, 0)

# count flickr visitor days inside PA per country per year
count_FVD_PA <- data.frame(table(flickr_PA$Country, flickr_PA$year, flickr_PA$PA))

names(count_FVD_PA) <- c("country", "year", "PA", "photos")

write.table(count_FVD_PA, "Photos_in_PA.txt", sep = "\t", row.names = F)

# reshape in wide format so we have one column for photos in and one column for photos out of PA
photos_PA <- reshape(count_FVD_PA, idvar = c("country", "year"), timevar = "PA", direction = "wide")
# rename columns
names(photos_PA)[3:4] <- c("photos_out", "photos_in")
# calculate total number of photos
photos_PA$photos_all <- photos_PA$photos_out + photos_PA$photos_in
# calculate proportion of photos in
photos_PA$prop_photos_in <- photos_PA$photos_in / photos_PA$photos_all

