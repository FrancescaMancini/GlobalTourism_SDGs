#############################################################
# Script for running Invariant Causal Predictions
# analysis on combined data (SDG indicators, WTO and Flickr) 
# Author: Francesca Mancini
# Date created: 2017-11-07
# Date modified: 2017-11-07
################################################## 
library(InvariantCausalPrediction)
library(seqICP)

combined <- read.table("combined.txt", sep = "\t", header = T)

# some countries in the SDG indicators dataset are grouped
# in global regions (East Asia, Europe etc.)
# and income levels (high, low, middle etc.)
# These groups are not present in the other datasets
# so we want to exclude them before the analysis

# make country_code a character string
combined$country_code <- as.character(combined$country_code)

# codes for grouped countries
grouped <- c("ARB", "CSS", "CEB", "EAR", "EAS", "EAP", "TEA", "EMU", "ECS", 
             "ECA", "TEC", "EUU", "FCS", "HPC", "HIC", "IBD", "IBT", "IDB", 
             "IDX", "IDA", "LTE", "LCN", "LAC", "TLA", "LDC", "LMY", "LIC", 
             "LMC", "MEA", "MNA", "TMN", "MIC", "NAC", "OED", "OSS", "PSS", 
             "PST", "PRE", "SST", "SAS", "TSA", "SSF", "SSA", "TSS", "UMC", "WLD")

# delete grouped countries from combined dataset             
combined_sub <- combined[-which(combined$country_code %in% grouped),]

# delete observations before 2004 and after 2014 (period for which we have flickr data)
combined_sub <- combined_sub[-which(combined_sub$year<2004 | combined_sub$year>2014),]

# make country_code a factor again
combined_sub$country_code <- as.factor(combined_sub$country_code)

ICP(as.matrix(combined_sub[,3:12]), combined_sub$AG.LND.FRST.K2, ExpInd = combined_sub$year)

Y <- combined_sub$NY.GDP.MKTP.KD.ZG[which(is.na(combined_sub$NY.GDP.MKTP.KD.ZG)!=T)]
X <- combined_sub[which(is.na(combined_sub$NY.GDP.MKTP.KD.ZG)!=T),c(3:12, 14:80)]


seqICP(as.matrix(X), Y, par.test = list(grid = seq(0, nrow(X), nrow(X)/244)), model = "ar", silent = FALSE)

seqICP.s(as.matrix(X), Y, S = numeric(), par.test = list(grid = seq(0, nrow(X), nrow(X)/244)), model = "ar")
