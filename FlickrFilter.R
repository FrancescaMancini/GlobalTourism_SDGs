#################################################
#Script for filtering 100M flickr 
#dataset according to concept tags
#Author: Francesca Mancini
#Date created: 2017-10-17
#Date modified: 2017-10-23
#################################################

# load packages and set-up----

library(R.utils)
library(data.table)

# create variable for file path
dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/Flickr100M/"

# unzip original data files
bunzip2("E:/Flickr1M/yfcc100m_autotags.bz2", 
        paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""))

for (j in 1:10) {
 bunzip2(paste(dataFilePath, "dataset", j, ".bz2", sep = ""),
         paste(dataFilePath, "dataset", j, ".txt", sep = ""), remove = T)}



# create a list of IDs for photos of nature---- 

# split up the dataset into 10 datasets to avoid running out of memory
# read in the first 10000000 lines
autotags1 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""), 
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000)

# assign column names
names(autotags1) <- c("photoID", "autotags")

# eliminate punctuation in the autotags column
autotags1$autotags <- gsub("[[:punct:] ]+", "", autotags1$autotags)
# eliminate numbers in the autotags column
autotags1$autotags <- gsub("[[:digit:]]+", " ", autotags1$autotags)

# create a vector with IDs of the photos containing the autotag "nature"
ID_sub1 <- autotags1[grep("nature", autotags1$autotags, value = F, perl = T),1]
rm(autotags1)
gc()

# save the vector as .txt file
write.table(ID_sub1, paste(dataFilePath, "ID_sub1.txt", sep = ""))
rm(ID_sub1)
gc()

# repeat for the rest of the dataset
autotags2 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"), sep="\t", nrows = 10000000, skip = 10000000)

names(autotags2) <- c("photoID", "autotags")

autotags2$autotags <- gsub("[[:punct:] ]+", "", autotags2$autotags)
autotags2$autotags <- gsub("[[:digit:]]+", " ", autotags2$autotags)

ID_sub2 <- autotags2[grep("nature", autotags2$autotags, value = F, perl = T),1]
rm(autotags2)
gc()

write.table(ID_sub2, paste(dataFilePath, "ID_sub2.txt", sep = ""))
rm(ID_sub2)
gc()



autotags3 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 20000000)
names(autotags3) <- c("photoID", "autotags")

autotags3$autotags <- gsub("[[:punct:] ]+", "", autotags3$autotags)
autotags3$autotags <- gsub("[[:digit:]]+", " ", autotags3$autotags)

ID_sub3 <- autotags3[grep("nature", autotags3$autotags, value = F, perl = T),1]
rm(autotags3)
gc()

write.table(ID_sub3, paste(dataFilePath, "ID_sub3.txt", sep = ""))
rm(ID_sub3)
gc()



autotags4 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 30000000)
names(autotags4) <- c("photoID", "autotags")

autotags4$autotags <- gsub("[[:punct:] ]+", "", autotags4$autotags)
autotags4$autotags <- gsub("[[:digit:]]+", " ", autotags4$autotags)

ID_sub4 <- autotags4[grep("nature", autotags4$autotags, value = F, perl = T),1]
rm(autotags4)
gc()

write.table(ID_sub4, paste(dataFilePath, "ID_sub4.txt", sep = ""))
rm(ID_sub4)
gc()



autotags5 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 40000000)
names(autotags5) <- c("photoID", "autotags")

autotags5$autotags <- gsub("[[:punct:] ]+", "", autotags5$autotags)
autotags5$autotags <- gsub("[[:digit:]]+", " ", autotags5$autotags)

ID_sub5 <- autotags5[grep("nature", autotags5$autotags, value = F, perl = T),1]
rm(autotags5)
gc()

write.table(ID_sub5, paste(dataFilePath, "ID_sub5.txt", sep = ""))
rm(ID_sub5)
gc()



autotags6 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 50000000)
names(autotags6) <- c("photoID", "autotags")

autotags6$autotags <- gsub("[[:punct:] ]+", "", autotags6$autotags)
autotags6$autotags <- gsub("[[:digit:]]+", " ", autotags6$autotags)

ID_sub6 <- autotags6[grep("nature", autotags6$autotags, value = F, perl = T),1]
rm(autotags6)
gc()

write.table(ID_sub6, paste(dataFilePath, "ID_sub6.txt", sep = ""))
rm(ID_sub6)
gc()



autotags7 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 60000000)
names(autotags7) <- c("photoID", "autotags")

autotags7$autotags <- gsub("[[:punct:] ]+", "", autotags7$autotags)
autotags7$autotags <- gsub("[[:digit:]]+", " ", autotags7$autotags)

ID_sub7 <- autotags7[grep("nature", autotags7$autotags, value = F, perl = T),1]
rm(autotags7)
gc()

write.table(ID_sub7, paste(dataFilePath, "ID_sub7.txt", sep = ""))
rm(ID_sub7)
gc()



autotags8 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 70000000)
names(autotags8) <- c("photoID", "autotags")

autotags8$autotags <- gsub("[[:punct:] ]+", "", autotags8$autotags)
autotags8$autotags <- gsub("[[:digit:]]+", " ", autotags8$autotags)

ID_sub8 <- autotags8[grep("nature", autotags8$autotags, value = F, perl = T),1]
rm(autotags8)
gc()

write.table(ID_sub8, paste(dataFilePath, "ID_sub8.txt", sep = ""))
rm(ID_sub8)
gc()


autotags9 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                   colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 80000000)
names(autotags9) <- c("photoID", "autotags")

autotags9$autotags <- gsub("[[:punct:] ]+", "", autotags9$autotags)
autotags9$autotags <- gsub("[[:digit:]]+", " ", autotags9$autotags)

ID_sub9 <- autotags9[grep("nature", autotags9$autotags, value = F, perl = T),1]
rm(autotags9)
gc()

write.table(ID_sub9, paste(dataFilePath, "ID_sub9.txt", sep = ""))
rm(ID_sub9)
gc()


autotags10 <- fread(paste(dataFilePath, "yfcc100m_autotags.txt", sep = ""),
                    colClasses = c("char", "char"),sep="\t", nrows = 10000000, skip = 90000000)
names(autotags10) <- c("photoID", "autotags")

autotags10$autotags <- gsub("[[:punct:] ]+", "", autotags10$autotags)
autotags10$autotags <- gsub("[[:digit:]]+", " ", autotags10$autotags)

ID_sub10 <- autotags10[grep("nature", autotags10$autotags, value = F, perl = T),1]
rm(autotags10)
gc()

write.table(ID_sub10, paste(dataFilePath, "ID_sub10.txt", sep = ""))
rm(ID_sub10)
gc()

# subset the complete dataset by retaining only photos with IDs selected in the previous part----


# create a dataset containing all the selected IDs
filename <- "ID_sub"
ID <- NULL

for (i in 1:10){
  ID <- rbind(ID, read.table(paste(dataFilePath, filename, i, ".txt", sep = ""),
                             colClasses = c("NULL", "character"), skip = 1, header = F))
}

names(ID) <- "photoID"

# save the dataset
# write.table(ID, paste(dataFilePath, "filteredID.txt", sep = ""))



# ID <- fread(paste(dataFilePath, "filteredID.txt", sep = ""),
#                  colClasses = c("NULL", "character"), skip = 1, header = F)
# names(ID) <- "photoID"

# create a list with all the files' names of the complete yfcc100m datasets

files <- list.files(dataFilePath, "dataset")

# apply a custom function to all the elements in the file list
# the function:
# 1 - reads the dataset in
# 2 - assigns column names
# 3 - filters the rows according to the photoIDs contained in the dataset ID using data.table indexes
# lapply creates a list containing the subsets of all 10 datasets with rows selected by the function

df_list <- lapply(files, function(x, ID){
  dat <- fread(paste(dataFilePath, x, sep = ""), header = F, sep = "\t", 
               colClasses=c("character", "character","NULL","character","NULL","NULL","NULL","NULL",
                            "NULL","NULL","numeric","numeric","NULL","NULL","NULL","NULL","NULL",
                            "NULL","NULL","NULL", "NULL","NULL","NULL"))
  
  names(dat) <- c("photoID", "userID", "date", "longitude", "latitude")
  setkey(dat, photoID)
  dat_filtered <- NULL
  dat_filtered<- rbind(dat_filtered, dat[.(ID[,1]), nomatch = 0L])
  
  return(dat_filtered)        
}, ID = ID)

# create one dataset by binding all the elements in the list (22644780 observations)
df_filtered <- do.call(rbind, df_list)

# discard rows with no lat/long (12935520 observations remaining)
df_filtered_sp <- df_filtered[which(is.na(df_filtered$longitude)!=TRUE),]

# save the dataset containing  observations
write.table(df_filtered_sp, paste(dataFilePath, "filteredData.txt", sep = ""), row.names = F)
