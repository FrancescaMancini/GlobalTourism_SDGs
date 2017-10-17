#################################################
#Script for filtering 100M flickr 
#dataset according to concept tags
#Author: Francesca Mancini
#Date created: 2017-10-17
#Date modified: 
#################################################

library(R.utils)
library(data.table)

tags <- gunzip("E:/Flickr1M/yfcc100m_autotags.bz2","C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt")

tags <- fread("E:/Flickr1M/yfcc100m_autotags.txt")
