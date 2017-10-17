#################################################
#Script for filtering 100M flickr 
#dataset according to concept tags
#Author: Francesca Mancini
#Date created: 2017-10-17
#Date modified: 
#################################################

library(R.utils)
library(data.table)

bunzip2("E:/Flickr1M/yfcc100m_autotags.bz2",
        "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt")

autotags1 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                      sep="\t", nrows = 10000000)
write.table(autotags1, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags1.txt")
rm(autotags1)


autotags2 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 10000000)
write.table(autotags2, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags2.txt")
rm(autotags2)


autotags3 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 20000000)
write.table(autotags3, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags3.txt")
rm(autotags3)


autotags4 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 30000000)
write.table(autotags4, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags4.txt")
rm(autotags4)


autotags5 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 40000000)
write.table(autotags5, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags5.txt")
rm(autotags5)


autotags6 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 50000000)
write.table(autotags6, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags6.txt")
rm(autotags6)


autotags7 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 60000000)
write.table(autotags7, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags7.txt")
rm(autotags7)


autotags8 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 70000000)
write.table(autotags8, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags8.txt")
rm(autotags8)


autotags9 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 80000000)
write.table(autotags9, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags9.txt")
rm(autotags9)


autotags1 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags.txt",
                        sep="\t", nrows = 10000000, skip = 90000000)
write.table(autotags10, "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags10.txt")
rm(autotags10)


tags1 <- read.table("C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/yfcc100m_autotags1.txt",
                    sep="\t")

names(tags1) <- c("photoID", "autotags")

tags1 <- gsub("[[:punct:] ]+", "", tags1)
tags1 <- gsub("[[:digit:]]+", "", tags1)

ID_sub1 <- tags1[grep("nature", tags1$autotags, value = F, perl = T),1]

write.table(ID_sub1, "ID_sub1.txt")


