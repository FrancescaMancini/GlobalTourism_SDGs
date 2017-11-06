##################################################
# Script for combining data from
# UNWTO, WB and Flickr 
# Author: Francesca Mancini
# Date created: 2017-10-27
# Date modified: 2017-11-03
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
NFVD_prop$NFVD_prop <- (NFVD_prop$NFVD/NFVD_prop$FVD)
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
flickr$NFVD_prop <- flickr$NFVD/flickr$FVD


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
