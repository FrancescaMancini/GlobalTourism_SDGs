#################################################
# Script for aggregating filtered flickr  
# pictures by country and year
# Author: Francesca Mancini
# Date created: 2017-10-23
# Date modified: 2017-11-03
#################################################

#####################################################
# this code was run on the University of Aberdeen 
# High Performance Computer Cluster Maxwell
# on a single node with 32G memory
#####################################################

library(data.table)
library(sp)
library(rworldmap)
library(lubridate)

# we want to calculate the proportion of all the 
# visitor days that are nature VD for each country

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)

summary(world)

##########################################
# first aggregate all pictures by country
##########################################

# create a list with all the files' names of the complete yfcc100m datasets
files <- list.files("./Flickr/yahoo100m/", "dataset")

# apply a custom function to all the elements in the file list
# the function aggregates all the photos in the 10 datasets by country
Flickr.all <- lapply(files, function(x, worldmap){
# read the dataset  
  dat <- fread(paste("./Flickr/yahoo100m/", x, sep = ""), header = F, sep = "\t", 
               colClasses=c("character", "character","NULL","character","NULL","NULL","NULL","NULL",
                            "NULL","NULL","numeric","numeric","NULL","NULL","NULL","NULL","NULL",
                            "NULL","NULL","NULL", "NULL","NULL","NULL"))
# assign column names  
  names(dat) <- c("photoID", "userID", "date", "longitude", "latitude")
# delete photos with no geotags
  dat <- dat[which(is.na(dat$longitude)!=TRUE),]
# make it spatial
  coordinates(dat) <- c("longitude", "latitude")
# assign same projection as world map
  proj4string(dat) <- proj4string(worldmap)
# overlay points over polygons
# (which country is every picture from?)
  country.dat <- over(dat, world)
# add a column in the original dataframe 
#  to store the country
  dat <- cbind(dat@data, country.dat$ADMIN)
# change column name
  names(dat)[4] <- "Country"
# return a list containing all the aggregated data 
# per country from the 10 datasets
  return(dat)        
}, worldmap = world)

# create one dataset by binding all the elements in the list 
dat <- do.call(rbind, Flickr.all)

#####################################################
# calculate how many flickr visitor days per country
#####################################################

# some observations do not have a date
# delete rows where date is null
dat <- dat[-which(dat$date=="null"),]
# create variable year
dat$year<-year(ymd_hms(dat$date))
summary(dat$year)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.
# 1    2008      2010      2010    2012    8981

# some of the dates were misreported
# delete all those later than 2014
dat <- dat[-which(dat$year>2014),]
# and all those earlier than 2004 (Flickr launch year)
dat <- dat[-which(dat$year<2004),]

# divide date from time
dat$dateonly <- as.character(sapply(strsplit(dat$date, " "), "[[", 1))   
# find multiple pictures from the same photographer in the same day and in the same country
d <- duplicated(dat[,c(2, 4, 6)])                        
# eliminate duplicates
dat_VD <- dat[-which(d=="TRUE"),]               

# delete photos at sea
dat_VD_terr <- dat_VD[-which(is.na(dat_VD$Country)==TRUE),]
# count visitor days per country
count_VD <- data.frame(table(dat_VD_terr$Country))
# give country column the same name as in the world dataset
names(count_VD)[1]<- "ADMIN"
# save
write.table(count_VD, "./Flickr/FVD_agg.txt",
            row.names = F, sep = "\t")

# count visitor days per country per year
count_VD_year <- data.frame(table(dat_VD_terr$Country, dat_VD_terr$year))
# assign column names
names(count_VD_year) <- c("country", "year", "FVD")
# save
write.table(count_VD_year, "./Flickr/FVD_year.txt",
            row.names = F, sep = "\t")


################################################
# now aggregate nature pictures by country
################################################

# read the data
flickr_nat <- fread("./Flickr/filteredData.txt", header = T, sep = "\t", 
      colClasses=c("character", "character","character", "numeric","numeric"))

# make it spatial
coordinates(flickr_nat) <- c("longitude", "latitude")
# assign same projection as world map
proj4string(flickr_nat) <- proj4string(world)
# plot on the map
plot(flickr_nat, pch = 20, col = "steelblue")
plot(flickr_nat, add = T)

# overlay points over polygons 
# (which country is every picture from?)
flickr_nat_country <- over(flickr_nat, world)
# add a column in the original dataframe 
# to store the country
flickr_nat <- cbind(flickr_nat@data, flickr_nat_country$ADMIN)
# change column name
names(flickr_nat)[4] <- "Country"

###############################################################
## calculate how many flickr nature visitor days per country ##
###############################################################

# create variable year
flickr_nat$year<-year(ymd_hms(flickr_nat$date))             

# some observations do not have a date
# delete rows where date is null

flickr_nat <- flickr_nat[-which(flickr_nat$date=="null"),]

summary(flickr_nat$year)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1    2008    2010    2010    2012    8981 

# some of the dates were misreported
# delete all those later than 2014
flickr_nat <- flickr_nat[-which(flickr_nat$year>2014),]

# and all those earlier than 2004 (Flickr launch year)
flickr_nat <- flickr_nat[-which(flickr_nat$year<2004),]

# divide date from time
flickr_nat$dateonly <- as.character(sapply(strsplit(flickr_nat$date, " "), "[[", 1))   
# find multiple pictures from the same photographer in the same day and in the same country
d_nat <- duplicated(flickr_nat[,c(2, 4, 6)])                        
# eliminate duplicates
flickr_nat_VD <- flickr_nat[-which(d_nat=="TRUE"),]               

# delete photos at sea
nat_VD_terr <- flickr_nat_VD[-which(is.na(flickr_nat_VD$Country)==TRUE),]

# count nature visitor days per country
count_NVD <- data.frame(table(nat_VD_terr$Country))
# give country column the same name as in the world dataset
names(count_NVD)[1]<- "ADMIN"
# save
write.table(count_NVD, "./Flickr/NFVD_agg.txt",
            row.names = F, sep = "\t")


# count nature visitor days per country per year
count_NVD_year <- data.frame(table(nat_VD_terr$Country, nat_VD_terr$year))

names(count_NVD_year) <- c("country", "year", "FUD")

write.table(count_NVD_year, "./Flickr/NFVD_year.txt",
            row.names = F, sep = "\t")

##############################################################
############### end of R code run in Maxwell #################
##############################################################


#################################################################
# make a map to visualise number of VD, NVD and prop per country
#################################################################


library(plyr)
library(ggplot2)
library(viridis)
#library(RColorBrewer)
#library(sp)
library(rworldmap)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)

# change names of count columns 
names(count_VD)[2] <- "FVD"
names(count_NVD)[2] <- "NFVD"

# merge the datasets so every country in the world dataset has the count of VD and NVD
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, count_NVD, by = "ADMIN", all.x = TRUE) 
world@data <- merge (world@data, count_VD, by = "ADMIN", all.x = TRUE)
# make NAs 0s
world@data$NFVD[which(is.na(world@data$NFVD)==TRUE)] <- 0

# calculate proportion of nature VD over all VD
world@data$NFVD_prop <- rep(NA, length(world@data$NFVD))

for(i in 1:length(world@data$FVD)){
if(world@data$FVD[i] != 0) {
  world@data$NFVD_prop[i] <- world@data$NFVD[i]/world@data$FVD[i]} 
}

# convert world map into a format readable by ggplot2
world_df <- fortify(world)
# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

# plot how many FVD per country

flickr_global_FVD <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      log10(FVD)), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis(option = "A")+
  #scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

tiff(filename="FlickrGlobalVD.tiff",width=3000,height=3000,res=300)
flickr_global_FVD
dev.off()


# NFVD
flickr_global_NFVD <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      log10(NFVD)), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis(option = "B")+
  #scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

tiff(filename="FlickrGlobalNVD.tiff",width=3000,height=3000,res=300)
flickr_global_NFVD
dev.off()

# NFVD proportion
flickr_global_NFVD_prop <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      NFVD_prop), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis()+
  #scale_fill_gradientn(colours=brewer.pal(n=8, name="PuBuGn")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

tiff(filename="FlickrGlobalNVDprop.tiff",width=3000,height=3000,res=300)
flickr_global_NFVD_prop
dev.off()


####################################################################
# make a map to visualise number of NVD prop per country per year
####################################################################

world <- getMap(resolution = "coarse")

# change names of count columns 
names(count_VD_year)[c(1,3)] <- c("ADMIN", "FVD")
names(count_NVD_year)[c(1,3)] <- c("ADMIN", "NFVD")

# merge the datasets so every country in the world dataset has the count of VD and NVD
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, count_NVD_year, by = "ADMIN", all.x = TRUE) 
world@data <- merge (world@data, count_VD_year, by = c("ADMIN", "year"), all.y = TRUE)
# make NAs 0s
world@data$NFVD[which(is.na(world@data$NFVD)==TRUE)] <- 0

# calculate proportion of nature VD over all VD
world@data$NFVD_prop <- rep(NA, length(world@data$NFVD))

for(i in 1:length(world@data$FVD)){
  if(world@data$FVD[i] != 0) {
    world@data$NFVD_prop[i] <- world@data$NFVD[i]/world@data$FVD[i]} 
}

# convert world map into a format readable by ggplot2
world_df <- fortify(world)
# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

# plot how many FVD per country

NFVD_prop_year <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      NFVD_prop), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis(option = "A", na.value = "white")+
  facet_wrap(~year)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

tiff(filename="FlickrNFVDprop.tiff",width=3000,height=3000,res=300)
NFVD_prop_year
dev.off()

