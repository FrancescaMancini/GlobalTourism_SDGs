#######################################
# Variable selection
# Author: Francesca Mancini
# Date created: 2018-01-04
# Date modified: 2018-01-08
#######################################

library(plyr)
library(ggplot2)
library(viridis)
library(rworldmap)

mydata <- read.table("integrated.txt", header = T)

mydata <- mydata[c(1:2,24:34,3:23,35:67)]

mydata <- mydata[,c(names(mydata)[1:12],sort(names(mydata)[13:67]))] 

# exclude all data for grouped countries (e.g. Europe, Asia etc.)

# make country_code a character string
mydata$country_code <- as.character(mydata$country_code)

# codes for grouped countries
grouped <- c("ARB", "CSS", "CEB", "EAR", "EAS", "EAP", "TEA", "EMU", "ECS", 
             "ECA", "TEC", "EUU", "FCS", "HPC", "HIC", "IBD", "IBT", "IDB", 
             "IDX", "IDA", "LTE", "LCN", "LAC", "TLA", "LDC", "LMY", "LIC", 
             "LMC", "MEA", "MNA", "TMN", "MIC", "NAC", "OED", "OSS", "PSS", 
             "PST", "PRE", "SST", "SAS", "TSA", "SSF", "SSA", "TSS", "UMC", "WLD")

# delete grouped countries from combined dataset             
mydata <- mydata[-which(mydata$country_code %in% grouped),]

# delete observations before 2004 and after 2014 (period for which we have flickr data)
mydata <- mydata[-which(mydata$year<2004 | mydata$year>2014),]

# make country_code a factor again
mydata$country_code <- as.factor(mydata$country_code)


# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)

# change names of count columns 
names(mydata)[1] <- "ISO3"


# merge the datasets so every country in the world dataset has the count of VD and NVD
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, mydata, by = "ISO3", all.x = TRUE) 
# make NAs 0s
#world@data$NFVD[which(is.na(world@data$NFVD)==TRUE)] <- 0


# convert world map into a format readable by ggplot2
world_df <- fortify(world)
# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

# plot how many international arrrivals per country

int_arrivals <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      arrivals_int), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis()+
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

tiff(filename="Int_arrivals.tiff",width=3000,height=3000,res=300)
int_arrivals
dev.off()


# create a function
 
country_year_maps <- function(data, value) {
  ggplot() + 
    geom_polygon(data = data, aes(x = long, y = lat, group = group, fill =
                                        value), colour = "black", size = 0.25) +
    coord_fixed() +
    scale_fill_viridis()+
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
  
}

country_year_maps(world_df, world_df$guests_int)

# plot all to check for variables with poor time and space coverage

# 