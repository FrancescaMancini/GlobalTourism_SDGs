##################################################
# Script for combining data from
# UNWTO, WB UN and Flickr 
# Author: Francesca Mancini
# Date created: 2017-10-27
# Date modified: 2018-01-11
##################################################

library(tidyr)
library(readr)

# save data directory as an object
dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/"

# read all data in
# number of flickr user days per country per year
FVD <- read.table(paste(dataFilePath, "Flickr100M/FVD_year.txt", sep = ""), header = T)
# number of flickr nature user days per country per year
NFVD <- read.table(paste(dataFilePath, "Flickr100M/NFVD_year.txt", sep = ""), header = T)
names(NFVD)[3] <- "NFVD"
# United Nations Sustainable Development Goals Indicators for goals 8, 12, 14 and 15
indicators <- read.table(paste(dataFilePath, "WBSDGindicators/SDG_IndicatorsData.txt", sep = ""), 
                         header = T, colClasses = c("factor", "factor", "character", "factor",
                                                    rep("numeric", 28)), na.string = "..", sep = "\t", quote="\"")
# WTO data on number of doemstic guests in accommodations per country per year
domestic_guests <- read.table(paste(dataFilePath, "UNWTO/Domestic_Accommodation_Guests.txt", sep = ""),
                              header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                              na.string = "..", sep = "\t", quote="\"")
# WTO data on numbre of trips by domestic tourists per country per year
domestic_trips <- read.table(paste(dataFilePath, "UNWTO/Domestic_Trips.txt", sep = ""),
                             header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                             na.string = "..", sep = "\t", quote="\"")
# WTO data on domestic visitors' expenditure
domestic_expenditure <- read.table(paste(dataFilePath, "UNWTO/Domestic_Indicators_Expenditure.txt", sep = ""),
                                   header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                                   na.string = "..", sep = "\t", quote="\"")
# WTO data on number of employees in tourism industries
employees <- read.table(paste(dataFilePath, "UNWTO/Employment_Employees_Number.txt", sep = ""),
                        header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                        na.string = "..", sep = "\t", quote="\"")
# WTO data on number of international tourists in accommodations
inbound_guest <- read.table(paste(dataFilePath, "UNWTO/Inbound_Accommodation_Guests.txt", sep = ""),
                            header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                            na.string = "..", sep = "\t", quote="\"")
# WTO data on number of international arrivals
inbound_arrivals <- read.table(paste(dataFilePath, "UNWTO/Inbound_Arrivals.txt", sep = ""),
                               header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                               na.string = "..", sep = "\t", quote="\"")
# WTO data on international tourists' expenditure 
inbound_expenditure <- read.table(paste(dataFilePath, "UNWTO/Inbound_Expenditure.txt", sep = ""),
                                  header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                                  na.string = "..", sep = "\t", quote="\"")
# WTO data on number of accommodation establishments
industry_accom_establish <- read.table(paste(dataFilePath, "UNWTO/Industries_Accommodation_Number.txt", sep = ""),
                                       header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                                       na.string = "..", sep = "\t", quote="\"")
# WTO data on monetary output of travel agencies
industry_agency_output <- read.table(paste(dataFilePath, "UNWTO/Industries_TourOp_Monetary.txt", sep = ""),
                                header = T, colClasses = c("numeric", "factor",rep("numeric", 21)),
                                na.string = "..", sep = "\t", quote="\"")


# tidy datasets
indicators <- gather(indicators, key = "year", value = "value", select = 5:32)
indicators$year <- parse_number(indicators$year)

write.table(indicators, paste(dataFilePath, "WBSDGindicators/indicators_tidy.txt", sep = ""), row.names = F)

domestic_guests <- gather(domestic_guests, key = "year", value = "guests_dom", select = 3:24)
domestic_guests$year <- parse_number(domestic_guests$year)

domestic_trips <- gather(domestic_trips, key = "year", value = "trips_dom", select = 3:24)
domestic_trips$year <- parse_number(domestic_trips$year)

domestic_expenditure <- gather(domestic_expenditure, key = "year", value = "exp_dom", select = 3:24)
domestic_expenditure$year <- parse_number(domestic_expenditure$year)

employees <- gather(employees, key = "year", value = "employ", select = 3:24)
employees$year <- parse_number(employees$year)

inbound_guest <- gather(inbound_guest, key = "year", value = "guests_int", select = 3:24)
inbound_guest$year <- parse_number(inbound_guest$year)

inbound_arrivals <- gather(inbound_arrivals, key = "year", value = "arrivals_int", select = 3:24)
inbound_arrivals$year <- parse_number(inbound_arrivals$year)

inbound_expenditure <- gather(inbound_expenditure, key = "year", value = "exp_int", select = 3:24)
inbound_expenditure$year <- parse_number(inbound_expenditure$year)

industry_accom_establish <- gather(industry_accom_establish, key = "year", value = "establishments", select = 3:24)
industry_accom_establish$year <- parse_number(industry_accom_establish$year)

industry_agency_output <- gather(industry_agency_output, key = "year", value = "agencies", select = 3:24)
industry_agency_output$year <- parse_number(industry_agency_output$year)

# put UNWTO data into one single dataframe
tourism <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(domestic_guests, domestic_trips, domestic_expenditure, 
                        employees, inbound_guest, inbound_arrivals,
                        inbound_expenditure, industry_accom_establish,
                        industry_agency_output))
write.table(tourism, paste(dataFilePath, "UNWTO/WTOcombined.txt", sep = ""), row.names = F)

# calculate proportion of nature flickr visitor days
NFVD_prop <- merge(NFVD, FVD, all = T)
NFVD_prop$NFVD[is.na(NFVD_prop$NFVD)] <- 0

NFVD_prop$NFVD_prop <- rep(NA, dim(NFVD_prop)[1])
  
for(i in 1:dim(NFVD_prop)[1]){
  if(NFVD_prop$NFVD[i]==0 & NFVD_prop$FVD[i]==0) NFVD_prop$NFVD_prop[i] <- 0
  else (NFVD_prop$NFVD_prop[i] <- NFVD_prop$NFVD[i]/NFVD_prop$FVD[i])}

write.table(NFVD_prop, paste(dataFilePath, "Flickr100M/NFVD_prop.txt", sep = ""), row.names = F)

##############################################
# combine tourism, flickr and indicators
##############################################
library(countrycode)
library(plyr)

tourism <- read.table(paste(dataFilePath, "UNWTO/WTOcombined.txt", sep = ""), header = T)
flickr <- read.table(paste(dataFilePath, "Flickr100M/NFVD_prop.txt", sep = ""), header = T)
indicators <- read.table(paste(dataFilePath, "WBSDGindicators/indicators_tidy.txt", sep = ""), header = T)

# names of countries are different in the 3 datasets
# need to transform into ISO codes
# first make all lowercase
tourism$COUNTRY <- as.factor(tolower(tourism$COUNTRY))
indicators$Country_Name <- as.factor(tolower(indicators$Country_Name))
flickr$country <- as.factor(tolower(flickr$country))

# convert flickr country names into ISOcodes
flickr$country_code <- countrycode(flickr$country, "country.name", "iso3c")
# Warning message:
#   In countrycode(flickr$country, "country.name", "iso3c") :
#   Some values were not matched unambiguously: ashmore and cartier islands, indian ocean territories, kosovo, saint martin, siachen glacier

# manually look for ISO codes and change them
flickr$country_code[which(flickr$country == "ashmore and cartier islands")] <- "AUS"

flickr$country_code[which(flickr$country == "indian ocean territories")] <- "IOT"

flickr$country_code[which(flickr$country == "kosovo")] <- "XKX"

flickr$country_code[which(flickr$country == "saint martin")] <- "MAF"

flickr$country_code[which(flickr$country == "siachen glacier")] <- "IND"

# now aggregate all columns with same country code and year
flickr <- ddply(flickr, c("country_code", "year"), numcolwise(sum), na.rm = T)
for(i in 1:dim(flickr)[1]){
  if(flickr$NFVD[i]==0 & flickr$FVD[i]==0) flickr$NFVD_prop[i] <- 0
  else (flickr$NFVD_prop[i] <- flickr$NFVD[i]/flickr$FVD[i])}



# convert tourism country names into ISO3 codes
tourism$country_code <- countrycode(tourism$COUNTRY, "country.name", "iso3c")
# Warning message:
#   In countrycode(tourism$COUNTRY, "country.name", "iso3c") :
#   Some values were not matched unambiguously: bonaire, saba, sint eustatius

# manually look for ISO codes and change them
tourism$country_code[which(is.na(tourism$country_code) == TRUE)] <- "BES"

# now aggregate all columns with same country code and year
# need to treat NAs in different ways:
# 1) as 0s when adding to a number, e.g. 1+NA=1
# 2) as NA when adding to NA, e.g. Na+NA=NA
# build a custom function for this
# and apply it to rows with same country code and year
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}

tourism <- ddply(tourism, c("country_code", "year"), numcolwise(plus))

# now merge datasets
tourism_combined <- merge (tourism, flickr, by = c("country_code", "year"), all = TRUE)
tourism_combined <- tourism_combined[,-c(3, 13, 14)]

write.table(tourism_combined, paste(dataFilePath, "UNWTO/tourism_vars.txt", sep = ""),
            sep = "\t", row.names = F)

# reshape indicators dataframe from long to wide
# so every indicator is a separate column
indicators <- indicators[,-c(1,3)]
indicators_wide <- spread(indicators, key = Series_Code, value = value)
names(indicators_wide)[1] <- "country_code"
combined <- merge (tourism_combined, indicators_wide, by = c("country_code", "year"), all = TRUE)

write.table(combined, "combined.txt", sep = "\t", row.names = F)



# fill the gaps in WB indicators dataset
# with UN indicators
indicators_UN <- read.csv(paste(dataFilePath, "UNSDGindicators/UN_indicators_combined.csv", sep = ""), header = T)

# only retain columns for indicators that we want to integrate
# load a reference table for indicators
UN_indic_legend <- read.table("UN_indicatorsLegend.txt", header = T)

# subset reference table
UN_indic_legend_sub1 <- subset(UN_indic_legend, Target %in% c("8.4", "12.4", "15.5", "15.6"))
UN_indic_legend_sub2 <- subset(UN_indic_legend, Indicator %in% 
                                 c("8.8.1", "8.8.4", "15.2.2", "15.2.3", 
                                   "15.2.5", "15.2.6", "15.4.1", "15.4.4"))

UN_indic_legend_sub <- rbind(UN_indic_legend_sub1, UN_indic_legend_sub2)

# use subset ref table to subset dataset
indicators_UN_sub <- subset(indicators_UN, select=names(indicators_UN) %in% UN_indic_legend_sub$Series.Code)
# and put year and country back in
indicators_UN_sub <- cbind(indicators_UN[1:2], indicators_UN_sub)

# change the coumn names to match target and indicator number
names(indicators_UN_sub) [-c(1,2)] <- paste("Indicator", UN_indic_legend_sub[match(names(indicators_UN_sub)[-c(1,2)], UN_indic_legend_sub$Series.Code),
                                                                     "Indicator"], sep = "-") 

# combine the two datasets
combined <- read.table("combined.txt", sep = "\t", header = T)

# first subset WB dataset (exclude all disaggregated indicators)
# load a reference table for indicators
WB_indic_legend <- read.table("WBindicatorsLegend.txt", header = T)
# subset reference table
WB_indic_legend_sub <- subset(WB_indic_legend, Indicator %in% 
                                 c("8.2.2", "8.2.3", "8.2.5", "8.2.6", "8.2.8", "8.2.9",
                                   "8.3.2", "8.3.3", "8.5.1", "8.5.2", "8.5.3", "8.5.4",
                                   "8.5.6", "8.5.7", "8.5.8", "8.5.9", "8.5.10", "8.5.12",
                                   "8.5.13", "8.5.14", "8.6.1", "8.6.2", "8.7.1", "8.7.2",
                                   "8.10.2", "8.10.3", "8.10.4", "8.10.5", "8.10.6", "8.10.7",
                                   "8.10.8", "8.10.9", "15.1.2", "15.1.3"))

# use subset ref table to subset dataset
combined_sub <- subset(combined, select=!(names(combined) %in% WB_indic_legend_sub$Series.Code))

# change the coumn names to match target and indicator number
names(combined_sub) [-c(1:12)] <- paste("Indicator", WB_indic_legend[match(names(combined_sub)[-c(1:12)], WB_indic_legend$Series.Code),
                                                                     "Indicator"], sep = "-") 
# rename country code column in UN indicators dataset
names(indicators_UN_sub)[1] <- "country_code"
# merge the two datasets
integrated <- merge (indicators_UN_sub, combined_sub, by = c("country_code", "year"), all.y = TRUE)

write.table(integrated, "integrated.txt", sep = "\t", row.names = F)

