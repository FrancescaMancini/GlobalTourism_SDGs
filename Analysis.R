# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
##### Script for graphical exploration and analysis ####
##### of SDG indicators and tourism
##### Author: Francesca Mancini
##### Date created: 2017-12-06
##### Date modified: 2018-04-03 
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #

# load packages
library(nlme)
library(gridExtra)
library(ggplot2)

# load the data and reorder the dataset columns
# so that indicators for the same target are close together

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

# delete observations before 1995 and after 2016 (period for which we have all data)
mydata <- mydata[-which(mydata$year<1995 | mydata$year>2016),]

# make country_code a factor again
mydata$country_code <- as.factor(mydata$country_code)

# add income level
income <- read.csv("Income_level.csv", header = T, colClasses = c("factor", "NULL", "factor"), sep = ",")
mydata <- merge (mydata, income, by = "country_code")

# =_=_=_=_=_=_=_=_=_=_=_ #
#### Transformations #####
# =_=_=_=_=_=_=_=_=_=_=_ #

# Tourism volume and income
par(mfrow = c(2,2))
#hist(mydata$trips_dom, breaks = 100)  
hist(mydata$arrivals_int, breaks = 100) 
hist(mydata$exp_int, breaks = 100) 
hist(mydata$NFVD_prop)  
dev.off()

par(mfrow = c(2,2))
hist(log(mydata$trips_dom), breaks = 100)  
hist(log(mydata$arrivals_int), breaks = 100) 
hist(log(mydata$exp_int), breaks = 100) 
hist(mydata$NFVD_prop)  
dev.off()

mydata.log <- mydata
#mydata.log$trips_dom <- log(mydata$trips_dom)
mydata.log$arrivals_int <- as.vector(scale(log(mydata$arrivals_int)))
mydata.log$exp_int <- as.vector(scale(log(mydata$exp_int + 1)))
mydata.log$NFVD_prop <- as.vector(scale(mydata$NFVD_prop))


# Tourism jobs

par(mfrow = c(1,2))
hist(mydata$employ, breaks = 100) 
hist(mydata$establishments, breaks = 100)   

dev.off()

mydata.log$employ <- as.vector(scale(log(mydata$employ)))
mydata.log$establishments <- as.vector(scale(log(mydata$establishments + 1)))


# Goal 8

par(mfrow = c(2,5))
hist(mydata$Indicator.8.1.1)
hist(mydata$Indicator.8.1.2)
hist(mydata$Indicator.8.2.1)
hist(mydata$Indicator.8.2.4)
hist(mydata$Indicator.8.2.7)
hist(mydata$Indicator.8.2.11)
hist(mydata$Indicator.8.5.5)
hist(mydata$Indicator.8.5.11)
hist(mydata$Indicator.8.5.15)
hist(mydata$Indicator.8.10.10)

summary(mydata$Indicator.8.1.1)
summary(mydata$Indicator.8.1.2)
summary(mydata$Indicator.8.2.1)
summary(mydata$Indicator.8.2.4)
summary(mydata$Indicator.8.2.7)
summary(mydata$Indicator.8.2.11)
summary(mydata$Indicator.8.5.5)
summary(mydata$Indicator.8.5.11)
summary(mydata$Indicator.8.5.15)
summary(mydata$Indicator.8.10.10)

par(mfrow = c(2,5))
hist(mydata$Indicator.8.1.1)
hist(mydata$Indicator.8.1.2)
hist(log(mydata$Indicator.8.2.1))
hist(mydata$Indicator.8.2.4)
hist(log(mydata$Indicator.8.2.7))
hist(mydata$Indicator.8.2.11)
hist(mydata$Indicator.8.5.5)
hist(mydata$Indicator.8.5.11)
hist(mydata$Indicator.8.5.15)
hist(log(mydata$Indicator.8.10.10))

mydata.log$Indicator.8.1.1 <- as.vector(scale(mydata$Indicator.8.1.1))
mydata.log$Indicator.8.1.2 <- as.vector(scale(mydata$Indicator.8.1.2))
mydata.log$Indicator.8.2.1 <- as.vector(scale(log(mydata$Indicator.8.2.1)))
mydata.log$Indicator.8.2.4 <- as.vector(scale(log(mydata$Indicator.8.2.4)))
mydata.log$Indicator.8.2.7 <- as.vector(scale(mydata$Indicator.8.2.7))
mydata.log$Indicator.8.2.11 <-  as.vector(scale(mydata$Indicator.8.2.11))
mydata.log$Indicator.8.5.5 <- as.vector(scale(mydata$Indicator.8.5.5))
mydata.log$Indicator.8.5.11 <- as.vector(scale(mydata$Indicator.8.5.11))
mydata.log$Indicator.8.5.15 <- as.vector(scale(mydata$Indicator.8.5.15))
mydata.log$Indicator.8.10.10 <- as.vector(scale(log(mydata$Indicator.8.10.10)))

# Goal 12

par(mfrow = c(2,4))
hist(mydata$Indicator.12.2.1)
hist(mydata$Indicator.12.2.2)
hist(mydata$Indicator.12.2.3)
hist(mydata$Indicator.12.2.4)
hist(mydata$Indicator.12.2.5)
hist(mydata$Indicator.12.2.6)
hist(mydata$Indicator.12.2.7)


summary(mydata$Indicator.12.2.1)
summary(mydata$Indicator.12.2.2)
summary(mydata$Indicator.12.2.3)
summary(mydata$Indicator.12.2.4)
summary(mydata$Indicator.12.2.5)
summary(mydata$Indicator.12.2.6)
summary(mydata$Indicator.12.2.7)

mydata.log$Indicator.12.2.1 <- as.vector(scale(mydata$Indicator.12.2.1))

# Goal 14

par(mfrow = c(2,2))
hist(mydata$Indicator.14.4.1, breaks = 50)
hist(mydata$Indicator.14.4.2, breaks = 50)
hist(mydata$Indicator.14.4.3, breaks = 50)
#hist(log(mydata.log$Indicator.14.5.1 + 1))


par(mfrow = c(2,2))
hist(log(mydata$Indicator.14.4.1 + 1))
hist(log(mydata$Indicator.14.4.2 + 1))
hist(log(mydata$Indicator.14.4.3 + 1))
#hist(log(mydata.log$Indicator.14.5.1 + 1))



summary(mydata$Indicator.14.4.1)
summary(mydata$Indicator.14.4.2)
summary(mydata$Indicator.14.4.3)
#summary(mydata.log$Indicator.14.5.1)


mydata.log$Indicator.14.4.1 <- as.vector(scale(log(mydata$Indicator.14.4.1 + 1)))
mydata.log$Indicator.14.4.2 <- as.vector(scale(log(mydata$Indicator.14.4.2 + 1)))
mydata.log$Indicator.14.4.3 <- as.vector(scale(log(mydata$Indicator.14.4.3 + 1)))


# Goal 15


par(mfrow = c(1,2))
hist(mydata$Indicator.15.1.1)
hist(mydata$Indicator.15.4.1)
hist(mydata.log$Indicator.15.5.1.x)

summary(mydata$Indicator.15.1.1)
summary(mydata$Indicator.15.4.1)
summary(mydata.log$Indicator.15.5.1.x)


mydata.log$Indicator.15.1.1 <- as.vector(scale(mydata$Indicator.15.1.1))
mydata.log$Indicator.15.4.1 <- as.vector(scale(mydata$Indicator.15.4.1))
mydata.log$Indicator.15.5.1 <- as.vector(scale(asin(mydata$Indicator.15.5.1.x))) 





# =_=_=_=_=_=_=_=_=_=_=_=_=_ #
# graphical exploration ######
# =_=_=_=_=_=_=_=_=_=_=_=_=_ #

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# look at correlation between indicators variables

# tourism
pairs(mydata.log[,c(6,8:10, 12)], upper.panel = panel.smooth, lower.panel = panel.cor,
      diag.panel = panel.hist, main = "Tourism") 

# goal 12
pairs(mydata.log[c(6,8:10, 12, 13)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Goal 12")

# goal 14
pairs(mydata.log[c(6,8:10, 12, 24:26)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Goal 14")

# goal 15
pairs(mydata.log[c(6,8:10, 12, 28, 35, 69)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Goal 15")


# target 8.1
pairs(mydata.log[c(6,8:10, 12, 47:48)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.1")

# target 8.2
pairs(mydata.log[c(6,8:10, 12, 51:55)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.2")

# target 8.5
pairs(mydata.log[c(6,8:10, 12, 61:63)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.5")

# target 8.10
pairs(mydata.log[c(6,8:10, 12, 50)], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.10")


ggplot(mydata, aes(x = year, y = exp_int)) +
  geom_line(aes(colour = country_code)) +
  geom_smooth(col = "black")

ggplot(mydata.log, aes(x = NFVD_prop, y = exp_int)) +
  geom_point(aes(colour = country_code)) +
  geom_smooth(aes(colour = country_code)) +
  facet_wrap(~income_level)

ggplot(mydata, aes(x = Indicator.8.2.11, y = Indicator.12.2.1)) +
  geom_point(aes(colour = income_level)) +
  geom_smooth(aes(colour = income_level))


# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
########### Tourism indicators #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
tourism_indicators <- subset(mydata.log, select = c(country_code, year, employ, arrivals_int, exp_int, establishments, NFVD_prop))
# delete observations before 2004 and after 2014 (period for which we have flickr data)
tourism_indicators <- tourism_indicators[-which(tourism_indicators$year<2004 | tourism_indicators$year>2014),]

tourism_indicators <- tourism_indicators[c(3:7,1,2)]


for(t_ind in 1:4){
  fmla <- as.formula(paste0(as.character(names(tourism_indicators)[t_ind]), "~", "NFVD_prop"))

  assign(paste("t_lme", names(tourism_indicators)[t_ind], sep = "_"), tryCatch(lme(fmla, data=tourism_indicators, random=~1|country_code,
                        correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")))
  
}

# need to obtain a table with all the results
# use lapply to obtain a table of coefficients 
# for each model and put them in a list
tourism_results <- lapply(ls(pattern="t_lme_"), function(x){as.data.frame(summary(get(x))$tTable)})

# change the names of list elements
names(tourism_results) <- ls(pattern="t_lme_")

# add a column for the explanatory variable name
tourism_results <- mapply(function(x) "[<-"(x, "indicator2", value = rownames(x)), tourism_results, SIMPLIFY = F)

# add a column for the response variable name
tourism_results <- Map(cbind, tourism_results, indicator1 = sapply(strsplit(names(tourism_results), "t_lme_"), "[[", 2))

# transform to a dataframe
tourism_results <- do.call(rbind, tourism_results)
row.names(tourism_results) <- NULL 
tourism_results <- tourism_results[c(7,6,1:5)]

# delete Intercepts
tourism_results <- subset(tourism_results, indicator2 !="(Intercept)")




# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
############## SDG8 indicators #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #

SDG8_indicators <- subset(mydata.log, select = c(Indicator.8.1.1 , Indicator.8.1.2, Indicator.8.2.1, 
                                                 Indicator.8.2.4, Indicator.8.2.7, Indicator.8.2.11,
                                                 Indicator.8.5.5, Indicator.8.5.11, Indicator.8.5.15,
                                                 Indicator.8.10.10, arrivals_int, employ, establishments,
                                                 exp_int, country_code, year))


for(sdg8_ind in 1:10){
  fmla_arrivals <- as.formula(paste0(as.character(names(SDG8_indicators)[sdg8_ind]), "~", "arrivals_int"))
  
  fmla_employment <- as.formula(paste0(as.character(names(SDG8_indicators)[sdg8_ind]), "~", "employ"))
  
  fmla_establishments <- as.formula(paste0(as.character(names(SDG8_indicators)[sdg8_ind]), "~", "establishments"))
  
  fmla_exp <- as.formula(paste0(as.character(names(SDG8_indicators)[sdg8_ind]), "~", "exp_int"))
  
  assign(paste("lme_", names(SDG8_indicators)[sdg8_ind], ".1", sep = ""), 
         tryCatch(lme(fmla_arrivals, data=SDG8_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")))
  
  assign(paste("lme_", names(SDG8_indicators)[sdg8_ind], ".2", sep = ""), 
         tryCatch(lme(fmla_employment, data=SDG8_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")))
  
  assign(paste("lme_", names(SDG8_indicators)[sdg8_ind], ".3", sep = ""), 
         tryCatch(lme(fmla_establishments, data=SDG8_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")))
  
  assign(paste("lme_", names(SDG8_indicators)[sdg8_ind], ".4", sep = ""), 
         tryCatch(lme(fmla_exp, data=SDG8_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")))
  
}

# need to obtain a table with all the results
# use lapply to obtain a table of coefficients 
# for each model and put them in a list
SDG8_results <- lapply(ls(pattern="lme_Indicator.8"), function(x){as.data.frame(summary(get(x))$tTable)})

# change the names of list elements
names(SDG8_results) <- ls(pattern="lme_Indicator.8")

# add a column for the explanatory variable name
SDG8_results <- mapply(function(x) "[<-"(x, "indicator2", value = rownames(x)), SDG8_results, SIMPLIFY = F)

# add a column for the response variable name
SDG8_results <- Map(cbind, SDG8_results, indicator1 = gsub("lme_(.*)\\..*", "\\1", names(SDG8_results), perl = T))

# transform to a dataframe
SDG8_results <- do.call(rbind, SDG8_results)
row.names(SDG8_results) <- NULL
SDG8_results <- SDG8_results[c(7,6,1:5)]

# delete Intercepts
SDG8_results <- subset(SDG8_results, indicator2 !="(Intercept)")



# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #
############## SDG12 indicators #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #
SDG12_indicators <- subset(mydata.log, select = c(Indicator.12.2.1, arrivals_int, employ, 
                                                 establishments, exp_int, country_code, year))



  fmla_arrivals <- as.formula("Indicator.12.2.1 ~ arrivals_int")
  
  fmla_employment <- as.formula("Indicator.12.2.1 ~ employ")
  
  fmla_establishments <- as.formula("Indicator.12.2.1 ~ establishments")
  
  fmla_exp <- as.formula("Indicator.12.2.1 ~ exp_int")
  
  lme_Indicator.12.2.1.1 <- lme(fmla_arrivals, data=SDG12_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")
 
  lme_Indicator.12.2.1.2 <- lme(fmla_employment, data=SDG12_indicators, random=~1|country_code,
                                correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")
  
  lme_Indicator.12.2.1.3 <- lme(fmla_establishments, data=SDG12_indicators, random=~1|country_code,
                                correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")
  
  lme_Indicator.12.2.1.4 <- lme(fmla_exp, data=SDG12_indicators, random=~1|country_code,
                                correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML")
  

# need to obtain a table with all the results
# use lapply to obtain a table of coefficients 
# for each model and put them in a list
SDG12_results <- lapply(ls(pattern="lme_Indicator.12"), function(x){as.data.frame(summary(get(x))$tTable)})

# change the names of list elements
names(SDG12_results) <- ls(pattern="lme_Indicator.12")

# add a column for the explanatory variable name
SDG12_results <- mapply(function(x) "[<-"(x, "indicator2", value = rownames(x)), SDG12_results, SIMPLIFY = F)

# add a column for the response variable name
SDG12_results <- Map(cbind, SDG12_results, indicator1 = gsub("lme_(.*)\\..*", "\\1", names(SDG12_results), perl = T))
                      #sapply(strsplit(names(SDG8_results), "t_lme""), "[[", 2))

# transform to a dataframe
SDG12_results <- do.call(rbind, SDG12_results)
row.names(SDG12_results) <- NULL 
SDG12_results <- SDG12_results[c(7,6,1:5)]

# delete Intercepts
SDG12_results <- subset(SDG12_results, indicator2 !="(Intercept)")



# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #
############## SDG14 indicators #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #

SDG14_indicators <- subset(mydata.log, select = c(Indicator.14.4.1, Indicator.14.4.2, Indicator.14.4.3, 
                                                 arrivals_int, employ, establishments,
                                                 exp_int, country_code, year))


for(sdg14_ind in 1:3){
  fmla_arrivals <- as.formula(paste0(as.character(names(SDG14_indicators)[sdg14_ind]), "~", "arrivals_int"))
  
  fmla_employment <- as.formula(paste0(as.character(names(SDG14_indicators)[sdg14_ind]), "~", "employ"))
  
  fmla_establishments <- as.formula(paste0(as.character(names(SDG14_indicators)[sdg14_ind]), "~", "establishments"))
  
  fmla_exp <- as.formula(paste0(as.character(names(SDG14_indicators)[sdg14_ind]), "~", "exp_int"))
  
  assign(paste("lme_", names(SDG14_indicators)[sdg14_ind], ".1", sep = ""), 
         tryCatch(lme(fmla_arrivals, data=SDG14_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG14_indicators)[sdg14_ind], ".2", sep = ""), 
         tryCatch(lme(fmla_employment, data=SDG14_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG14_indicators)[sdg14_ind], ".3", sep = ""), 
         tryCatch(lme(fmla_establishments, data=SDG14_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG14_indicators)[sdg14_ind], ".4", sep = ""), 
         tryCatch(lme(fmla_exp, data=SDG14_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
}

# need to obtain a table with all the results
# use lapply to obtain a table of coefficients 
# for each model and put them in a list
SDG14_results <- lapply(ls(pattern="lme_Indicator.14"), function(x){as.data.frame(summary(get(x))$tTable)})

# change the names of list elements
names(SDG14_results) <- ls(pattern="lme_Indicator.14")

# add a column for the explanatory variable name
SDG14_results <- mapply(function(x) "[<-"(x, "indicator2", value = rownames(x)), SDG14_results, SIMPLIFY = F)

# add a column for the response variable name
SDG14_results <- Map(cbind, SDG14_results, indicator1 = gsub("lme_(.*)\\..*", "\\1", names(SDG14_results), perl = T))
#sapply(strsplit(names(SDG8_results), "t_lme""), "[[", 2))

# transform to a dataframe
SDG14_results <- do.call(rbind, SDG14_results)
row.names(SDG14_results) <- NULL 
SDG14_results <- SDG14_results[c(7,6,1:5)]

# delete Intercepts
SDG14_results <- subset(SDG14_results, indicator2 !="(Intercept)")


# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #
############## SDG15 indicators #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_= #
SDG15_indicators <- subset(mydata.log, select = c(Indicator.15.1.1, Indicator.15.4.1, Indicator.15.5.1, 
                                                  arrivals_int, employ, establishments,
                                                  exp_int, country_code, year))


for(sdg15_ind in 1:3){
  fmla_arrivals <- as.formula(paste0(as.character(names(SDG15_indicators)[sdg15_ind]), "~", "arrivals_int"))
  
  fmla_employment <- as.formula(paste0(as.character(names(SDG15_indicators)[sdg15_ind]), "~", "employ"))
  
  fmla_establishments <- as.formula(paste0(as.character(names(SDG15_indicators)[sdg15_ind]), "~", "establishments"))
  
  fmla_exp <- as.formula(paste0(as.character(names(SDG15_indicators)[sdg15_ind]), "~", "exp_int"))
  
  assign(paste("lme_", names(SDG15_indicators)[sdg15_ind], ".1", sep = ""), 
         tryCatch(lme(fmla_arrivals, data=SDG15_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG15_indicators)[sdg15_ind], ".2", sep = ""), 
         tryCatch(lme(fmla_employment, data=SDG15_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG15_indicators)[sdg15_ind], ".3", sep = ""), 
         tryCatch(lme(fmla_establishments, data=SDG15_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
  assign(paste("lme_", names(SDG15_indicators)[sdg15_ind], ".4", sep = ""), 
         tryCatch(lme(fmla_exp, data=SDG15_indicators, random=~1|country_code,
                      correlation=corAR1(form=~year|country_code), na.action = na.omit, method = "ML",
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))))
  
}

# need to obtain a table with all the results
# use lapply to obtain a table of coefficients 
# for each model and put them in a list
SDG15_results <- lapply(ls(pattern="lme_Indicator.15"), function(x){as.data.frame(summary(get(x))$tTable)})

# change the names of list elements
names(SDG15_results) <- ls(pattern="lme_Indicator.15")

# add a column for the explanatory variable name
SDG15_results <- mapply(function(x) "[<-"(x, "indicator2", value = rownames(x)), SDG15_results, SIMPLIFY = F)

# add a column for the response variable name
SDG15_results <- Map(cbind, SDG15_results, indicator1 = gsub("lme_(.*)\\..*", "\\1", names(SDG15_results), perl = T))
#sapply(strsplit(names(SDG8_results), "t_lme""), "[[", 2))

# transform to a dataframe
SDG15_results <- do.call(rbind, SDG15_results)
row.names(SDG15_results) <- NULL 
SDG15_results <- SDG15_results[c(7,6,1:5)]

# delete Intercepts
SDG15_results <- subset(SDG15_results, indicator2 !="(Intercept)")



# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
############### Visualisations #################
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
library(qgraph) 

# tourism
# make results into a matrix
tourism_results_viz <- matrix(0, 5, 5)
rownames(tourism_results_viz) <- c(as.character(tourism_results$indicator1), "NFVD_prop")
colnames(tourism_results_viz) <- c(as.character(tourism_results$indicator1), "NFVD_prop")
tourism_results_viz[5,1:4] <- c(tourism_results$Value)


qgraph(tourism_results_viz, maximum=0.02, minimum=0.00025, borders = F, vsize = 20,
       node.resolution=400, directed = T, shape="circle", color = c("darkseagreen", "lavenderblush3"),
       #posCol="deepskyblue4", negCol="darkred", layout="groups", 
       vsize=10, fade = T, theme = "colorblind",
       groups = list(Tourism = 5, Flickr = c(1:4)), legend = F, edge.labels = T,
       layout = matrix(c(-.9,-.3,.3,.9, 0, -.7,-.7,-.7,-.7,.7), ncol = 2),
       labels = c("International \narrivals", "Employment", "Establishments", "Expenditure", "Nature tourism"),
       label.cex = 1, edge.label.cex = 2,filetype="pdf",filename="Tourism_indicators", width = 3.5, height = 3.5)



# SDG 8
# make results into a matrix
SDG8_results_viz <- matrix(0, 14, 14)
rownames(SDG8_results_viz) <- c(as.character(unique(SDG8_results$indicator1)), as.character(unique(SDG8_results$indicator2)))
colnames(SDG8_results_viz) <- c(as.character(unique(SDG8_results$indicator1)), as.character(unique(SDG8_results$indicator2)))
SDG8_results_viz[11:14,1:10] <- c(SDG8_results$Value)

qgraph(SDG8_results_viz, maximum=0.45, minimum=0, borders = F, vsize = 15, 
       node.resolution=400, directed = T, shape="circle", 
       vsize=10, fade = T, theme = "colorblind", color = c("lavenderblush3", "#8F1838", "#A5465F", "#C07D8F", "#D9B1BB"),
       groups = list(Tourism = c(11:14), Target_8.1 = c(1:2), Target_8.2 = c(4:7), Target_8.5 = c(8:10), Target_8.10 = 3), 
       layout = matrix(c(1,11,4,2,12,6,10,13,7,8,14,5,9,0,3), ncol = 5),
       labels = c("8.1.1", "8.1.2", "8.10.10", "8.2.1", "8.2.11", "8.2.4", "8.2.7", "8.5.11", "8.5.15", "8.5.5", 
                  "International \narrivals", "Employment", "Establishments", "Expenditure"),
       label.cex = 1, legend = F,filetype="pdf",filename="SDG8_indicators", width = 5, height = 4)




# SDG 12
# make results into a matrix
SDG12_results_viz <- matrix(0, 5, 5)
rownames(SDG12_results_viz) <- c(as.character(unique(SDG12_results$indicator1)), as.character(unique(SDG12_results$indicator2)))
colnames(SDG12_results_viz) <- c(as.character(unique(SDG12_results$indicator1)), as.character(unique(SDG12_results$indicator2)))
SDG12_results_viz[2:5,1] <- c(SDG12_results$Value)


qgraph(SDG12_results_viz, maximum=0.3, minimum=0, borders = F, vsize = 20, 
       node.resolution=400, directed = T, shape="circle",
       vsize=10, fade = T, theme = "colorblind", color = c("lavenderblush3", "#CF8D2A"),
       groups = list(covariate = c(2:5), response = 1), 
       layout = matrix(c(0,-.9,-.3,.3,.9, -.8,.7,.7,.7,.7), ncol = 2),
       legend = F, edge.labels = T,
       labels = c("12.2.1", "International \narrivals", "Employment", "Establishments", "Expenditure"),
       label.cex = 1, edge.label.cex = 2,filetype="pdf",filename="SDG12_indicator", width = 3.5, height = 3.5)



# SDG 14
# make results into a matrix
SDG14_results_viz <- matrix(0, 7, 7)
rownames(SDG14_results_viz) <- c(as.character(unique(SDG14_results$indicator1)), as.character(unique(SDG14_results$indicator2)))
colnames(SDG14_results_viz) <- c(as.character(unique(SDG14_results$indicator1)), as.character(unique(SDG14_results$indicator2)))
SDG14_results_viz[4:7,1:3] <- c(SDG14_results$Value)


qgraph(SDG14_results_viz, maximum=0.2, minimum=0, borders = F, vsize = 15, 
       node.resolution=400, directed = T, shape="circle", 
       vsize=10, fade = T, theme = "colorblind", color = c("lavenderblush3", "#1F97D4"),
       groups = list(covariate = c(4:7), response = c(1:3)), 
       legend = F, edge.labels = T, edge.label.position = c(.2,.2,.2,.2,.4,.4,.4,.4,.6,.6,.6,.6),
       layout = matrix(c(-.6,0,.6,-.9,-.3,.3,.9,-.8,-.8,-.8,.8,.8,.8,.8), ncol = 2),
       labels = c("14.4.1", "14.4.2", "14.4.3", 
                  "International \narrivals", "Employment", "Establishments", "Expenditure"),
       label.cex = 1, edge.label.cex = 2,filetype="pdf",filename="SDG14_indicators", width = 5, height = 4)



# SDG 15
# make results into a matrix
SDG15_results_viz <- matrix(0, 7, 7)
rownames(SDG15_results_viz) <- c(as.character(unique(SDG15_results$indicator1)), as.character(unique(SDG15_results$indicator2)))
colnames(SDG15_results_viz) <- c(as.character(unique(SDG15_results$indicator1)), as.character(unique(SDG15_results$indicator2)))
SDG15_results_viz[4:7,1:3] <- c(SDG15_results$Value)


qgraph(SDG15_results_viz, maximum=0.2, minimum=0, borders = F, vsize = 15, 
       node.resolution=400, directed = T, shape="circle",
       vsize=10, fade = T, theme = "colorblind", color = c("lavenderblush3", "#59BA47", "#8ACE7E", "#ADDCA4"),
       groups = list(Tourism = c(4:7), Target_15.1 = 1, Target_15.4 = 2, Target_15.5 = 3), 
       legend = F, edge.labels = T, edge.label.position = c(.2,.2,.2,.2,.4,.4,.4,.4,.6,.6,.6,.6),
       layout = matrix(c(-.6,0,.6,-.9,-.3,.3,.9,-.8,-.8,-.8,.8,.8,.8,.8), ncol = 2),
       labels = c("15.1.1", "15.4.1", "15.5.1", 
                  "International \narrivals", "Employment", "Establishments", "Expenditure"),
       label.cex = 1, edge.label.cex = 2, filetype="pdf",filename="SDG15_indicators", width = 5, height = 3.5)


# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ # 
## Effect of tourism on PA coverage ####
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ # 

library(plyr)

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

# make country_code a factor again
mydata$country_code <- as.factor(mydata$country_code)

# function to calculate yearly rate of change in time series
rateChange<- function(x) { 
  c(NA, (diff(x, 1)))
}

# exclude observations outside 2004-2014 period
mydata_sub <- mydata[-which(mydata$year<2004 | mydata$year>2014),]
# order dataset by year
mydata_sub <- mydata_sub[order(mydata_sub$year), ] 
# calculate yearly rate of change for every tourism time series and add it as a variable to mydata_sub
mydata_sub <- ddply(mydata_sub, "country_code", transform, arrivals_int_change = rateChange(arrivals_int))
mydata_sub <- ddply(mydata_sub, "country_code", transform, exp_int_change = rateChange(exp_int))
mydata_sub <- ddply(mydata_sub, "country_code", transform, employ_change = rateChange(employ))
mydata_sub <- ddply(mydata_sub, "country_code", transform, establish_change = rateChange(establishments))
mydata_sub <- ddply(mydata_sub, "country_code", transform, NFVD_change = rateChange(NFVD_prop))

# calculate the median yearly rate of change for each country and tourism time series
arrivals_int_change <- aggregate(mydata_sub$arrivals_int_change ~ 
                                   mydata_sub$country_code, FUN = median)

names(arrivals_int_change) <- c("country_code", "arrivals_int_change")

exp_int_change <- aggregate(mydata_sub$exp_int_change ~ 
                              mydata_sub$country_code, FUN = median)

names(exp_int_change) <- c("country_code", "exp_int_change")

employ_change <- aggregate(mydata_sub$employ_change ~ 
                             mydata_sub$country_code, FUN = median)

names(employ_change) <- c("country_code", "employ_change")

establish_change <- aggregate(mydata_sub$establish_change ~ 
                                mydata_sub$country_code, FUN = median)

names(establish_change) <- c("country_code", "establish_change")

NFVD_change <- aggregate(mydata_sub$NFVD_change ~
                           mydata_sub$country_code, FUN = median)

names(NFVD_change) <- c("country_code", "NFVD_change")


#### terrestrial PAs ####

# calcuate difference between PA coverage in 2014 and PA_coverage in 2004
PA_cov <- ddply(mydata, "country_code", summarize,
                PA_change = Indicator.15.1.4[year == 2014] -
                  Indicator.15.1.4[year == 2000])

# merge all into one dataset
PA_df <- Reduce(function(x, y) merge(x, y, by = "country_code", all=TRUE), 
                list(PA_cov, arrivals_int_change, exp_int_change, employ_change, establish_change, NFVD_change))


# visualise change in PA
# change in protected area coverage between 2004-2014
library(ggplot2)
library(viridis)
library(rworldmap)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
# plot(world)

# change names of count columns 
names(PA_df)[1] <- "ISO3"


# merge the datasets so every country in the world dataset has the count of VD and NVD
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, PA_df, by = "ISO3", all.x = TRUE) 
# make NAs 0s
#world@data$NFVD[which(is.na(world@data$NFVD)==TRUE)] <- 0


# convert world map into a format readable by ggplot2
world_df <- fortify(world)
# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

country_year_maps <- function(data, xcol, ycol, group, value) {
  map <- ggplot() + 
    geom_polygon(data = data, aes_string(x = xcol, y = ycol, group = group, fill =
                                           value), colour = "black", size = 0.25) +
    coord_fixed() +
    scale_fill_viridis()+
    #facet_wrap(~year)+
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
  
  plot(map)   
}

country_year_maps(data = world_df, xcol = "long", ycol = "lat", group = "group", value = "PA_change")


# analysis

# add income level
income <- read.csv("Income_level.csv", header = T, colClasses = c("factor", "NULL", "factor"), sep = ",")
PA_df <- merge (PA_df, income, by = "country_code")

hist(PA_df$PA_change)

plot(PA_change ~ arrivals_int_change, data = PA_df)
plot(PA_change ~ establish_change, data = PA_df)
plot(PA_change ~ NFVD_change, data = PA_df)
plot(PA_change ~ employ_change, data = PA_df)
plot(PA_change ~ exp_int_change, data = PA_df)


PA_change.lm <- lm(PA_change ~ arrivals_int_change + establish_change + NFVD_change, data = PA_df)

par(mfrow = c(2,2))
plot(PA_change.lm)

boxplot(PA_df$PA_change~PA_df$income_level)

summary(PA_change.lm)

PA_df$continent <- countrycode::countrycode(PA_df$country_code, "iso3c", "continent")
# Warning message:
#   In countrycode::countrycode(PA_df$country_code, "iso3c", "continent") :
#   Some values were not matched unambiguously: CHI, XKX

PA_df[which(PA_df$country_code=="ATA"), "continent"] <- "Antarctica"
PA_df[which(PA_df$country_code=="ATF"), "continent"] <- "Africa"
PA_df[which(PA_df$country_code=="CHI"), "continent"] <- "Americas"
PA_df[which(PA_df$country_code=="HMD"), "continent"] <- "Antarctica"
PA_df[which(PA_df$country_code=="IOT"), "continent"] <- "Asia"
PA_df[which(PA_df$country_code=="SGS"), "continent"] <- "Antarctica"
PA_df[which(PA_df$country_code=="XKX"), "continent"] <- "Europe"

PA_df$continent <- as.factor(PA_df$continent)

boxplot(PA_change ~ continent, data = PA_df)

hist(PA_df$arrivals_int_change, breaks = 10)
hist(PA_df$establish_change, breaks = 10)
hist(PA_df$exp_int_change, breaks = 10)
hist(PA_df$employ_change, breaks = 10)

PA_df$arrivals_int_change_cubrt <- (PA_df$arrivals_int_change)^(1/3)
PA_df$establish_change_cubrt <- (PA_df$establish_change)^(1/3)
PA_df$exp_int_change_cubrt <- (PA_df$exp_int_change)^(1/3)
PA_df$employ_change_cubrt <- (PA_df$employ_change)^(1/3)

hist(PA_df$arrivals_int_change_cubrt, breaks = 10)
hist(PA_df$establish_change_cubrt, breaks = 10)
hist(PA_df$exp_int_change_cubrt, breaks = 10)
hist(PA_df$employ_change_cubrt, breaks = 10)


PA_change.gls.1 <- gls(PA_change ~ arrivals_int_change_cubrt, data = PA_df, 
                       weights = varIdent(form = ~ 1 | continent), na.action = na.omit)


grid.arrange(plot(PA_change.gls.1, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(PA_change.gls.1, form = resid(., type = "n") ~ arrivals_int_change_cubrt, abline = 0),
             plot(PA_change.gls.1, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(PA_change.gls.1, type = "p"))
qqline(resid(PA_change.gls.1, type = "p"))


summary(PA_change.gls.1)


PA_change.gls.2 <- gls(PA_change ~ exp_int_change_cubrt , data = PA_df,
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)


grid.arrange(plot(PA_change.gls.2, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(PA_change.gls.2, form = resid(., type = "n") ~ exp_int_change_cubrt, abline = 0),
             plot(PA_change.gls.2, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(PA_change.gls.2, type = "p"))
qqline(resid(PA_change.gls.2, type = "p"))

summary(PA_change.gls.2)


PA_change.gls.3 <- gls(PA_change ~ employ_change_cubrt, data = PA_df,
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(PA_change.gls.3, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(PA_change.gls.3, form = resid(., type = "n") ~ employ_change_cubrt, abline = 0),
             plot(PA_change.gls.3, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(PA_change.gls.3, type = "p"))
qqline(resid(PA_change.gls.3, type = "p"))


summary(PA_change.gls.3)


PA_change.gls.4 <- gls(PA_change ~ establish_change_cubrt, data = PA_df,
                       weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(PA_change.gls.4, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(PA_change.gls.4, form = resid(., type = "n") ~ establish_change_cubrt, abline = 0),
             plot(PA_change.gls.4, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(PA_change.gls.4, type = "p"))
qqline(resid(PA_change.gls.4, type = "p"))


summary(PA_change.gls.4)

PA_change.gls.5 <- gls(PA_change ~ NFVD_change, data = PA_df,
                       weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(PA_change.gls.5, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(PA_change.gls.5, form = resid(., type = "n") ~ NFVD_change, abline = 0),
             plot(PA_change.gls.5, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(PA_change.gls.5, type = "p"))
qqline(resid(PA_change.gls.5, type = "p"))


summary(PA_change.gls.5)


#### marine PAs ####

MPA_cov <- ddply(mydata, "country_code", summarize,
                 MPA_change = Indicator.14.5.1[year == 2014] -
                   Indicator.14.5.1[year == 2000])


MPA_df <- Reduce(function(x, y) merge(x, y, by = "country_code", all=TRUE), 
                 list(MPA_cov, arrivals_int_change, exp_int_change, employ_change, establish_change, NFVD_change))



# change in protected area coverage between 2004-2014
library(ggplot2)
library(viridis)
library(rworldmap)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
# plot(world)

# change names of count columns 
names(MPA_df)[1] <- "ISO3"


# merge the datasets so every country in the world dataset has the count of VD and NVD
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, MPA_df, by = "ISO3", all.x = TRUE) 
# make NAs 0s
#world@data$NFVD[which(is.na(world@data$NFVD)==TRUE)] <- 0


# convert world map into a format readable by ggplot2
world_df <- fortify(world)
# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

country_year_maps <- function(data, xcol, ycol, group, value) {
  map <- ggplot() + 
    geom_polygon(data = data, aes_string(x = xcol, y = ycol, group = group, fill =
                                           value), colour = "black", size = 0.25) +
    coord_fixed() +
    scale_fill_viridis()+
    #facet_wrap(~year)+
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
  
  plot(map)   
}

country_year_maps(data = world_df, xcol = "long", ycol = "lat", group = "group", value = "MPA_change")


# analysis

MPA_df$arrivals_int_change_cubrt <- (MPA_df$arrivals_int_change)^(1/3)
MPA_df$establish_change_cubrt <- (MPA_df$establish_change)^(1/3)
MPA_df$exp_int_change_cubrt <- (MPA_df$exp_int_change)^(1/3)
MPA_df$employ_change_cubrt <- (MPA_df$employ_change)^(1/3)

MPA_df$continent <- countrycode::countrycode(MPA_df$country_code, "iso3c", "continent")
# Warning message:
#   In countrycode::countrycode(MPA_df$country_code, "iso3c", "continent") :
#   Some values were not matched unambiguously: ATA, ATF, CHI, HMD, IOT, SGS, XKX


MPA_df[which(MPA_df$country_code=="ATA"), "continent"] <- "Antarctica"
MPA_df[which(MPA_df$country_code=="ATF"), "continent"] <- "Africa"
MPA_df[which(MPA_df$country_code=="CHI"), "continent"] <- "Americas"
MPA_df[which(MPA_df$country_code=="HMD"), "continent"] <- "Antarctica"
MPA_df[which(MPA_df$country_code=="IOT"), "continent"] <- "Asia"
MPA_df[which(MPA_df$country_code=="SGS"), "continent"] <- "Antarctica"
MPA_df[which(MPA_df$country_code=="XKX"), "continent"] <- "Europe"

MPA_df$continent <- as.factor(MPA_df$continent)

boxplot(MPA_change ~ continent, data = MPA_df)




MPA_change.gls.1 <- gls(MPA_change ~ arrivals_int_change_cubrt, data = MPA_df, 
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)


grid.arrange(plot(MPA_change.gls.1, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(MPA_change.gls.1, form = resid(., type = "n") ~ arrivals_int_change_cubrt, abline = 0),
             plot(MPA_change.gls.1, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(MPA_change.gls.1, type = "p"))
qqline(resid(MPA_change.gls.1, type = "p"))


summary(MPA_change.gls.1)


MPA_change.gls.2 <- gls(MPA_change ~ exp_int_change_cubrt, data = MPA_df, 
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)


grid.arrange(plot(MPA_change.gls.2, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(MPA_change.gls.2, form = resid(., type = "n") ~ exp_int_change_cubrt, abline = 0),
             plot(MPA_change.gls.2, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(MPA_change.gls.2, type = "p"))
qqline(resid(MPA_change.gls.2, type = "p"))


summary(MPA_change.gls.2)


MPA_change.gls.3 <- gls(MPA_change ~ employ_change_cubrt, data = MPA_df, 
                          weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(MPA_change.gls.3, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(MPA_change.gls.3, form = resid(., type = "n") ~ employ_change_cubrt, abline = 0),
             plot(MPA_change.gls.3, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(MPA_change.gls.3, type = "p"))
qqline(resid(MPA_change.gls.3, type = "p"))


summary(MPA_change.gls.3)

MPA_change.gls.4 <- gls(MPA_change ~ establish_change_cubrt, data = MPA_df, 
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(MPA_change.gls.4, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(MPA_change.gls.4, form = resid(., type = "n") ~ establish_change_cubrt, abline = 0),
             plot(MPA_change.gls.4, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(MPA_change.gls.4, type = "p"))
qqline(resid(MPA_change.gls.4, type = "p"))


summary(MPA_change.gls.4)

MPA_change.gls.5 <- gls(MPA_change ~ NFVD_change, data = MPA_df, 
                        weights = varIdent(form = ~ 1 | continent), na.action = na.omit)

grid.arrange(plot(MPA_change.gls.5, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(MPA_change.gls.5, form = resid(., type = "n") ~ NFVD_change, abline = 0),
             plot(MPA_change.gls.5, form = resid(., type = "n") ~ fitted(.)|continent, abline = 0))

qqnorm(resid(MPA_change.gls.5, type = "p"))
qqline(resid(MPA_change.gls.5, type = "p"))


summary(MPA_change.gls.5)


# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
# Proportion of Flickr photos in PAs ###########
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #

library(tidyr)
library(readr)

# save data directory as an object
dataFilePath <- "C:/Users/r03fm14/OneDrive - University of Aberdeen/Data/WB_governance/"

# create a list with all the files' names of the governance indicators
files <- list.files(dataFilePath, ".txt")

# create a function to read in the dataset, tidy it and apply to all data files
governance <- lapply(files, function(x){
  # read each dataset in
  # skip 14 rows of title and legend
  # fill = T takes care of empty rows
  # need to specify NA string, but due to the "#" symbol we need to to turn off the interpretation 
  # of comments with comment.char otherwise it will interpret every column that starts with NA string as empty
  # we also skip the first column because it contains country names with special characters
  # quote = "" disables quoting characters
  dat <- read.table(paste(dataFilePath, x, sep = ""), skip = 14, na.strings = "#N/A", 
                    header = T, fill = T, sep = "\t", comment.char = "",
                    colClasses = c("NULL", "character", rep("numeric", 108)), quote = "")
  
  # only keep country codes and columns that start with "Estimate"
  dat <- dat[, c(1, grep("^Estimate", names(dat)))] 
  
  # rename columns
  names(dat) <- c("ISO3","1996", "1998", "2000", "2002", "2003", "2004", 
                  "2005", "2006", "2007", "2008", "2009", "2010", 
                  "2011", "2012", "2013", "2014", "2015", "2016")
  
  # make country code a factor
  dat$ISO3 <- as.factor(dat$ISO3)
  
  # transform to long format with one row per year
  dat <- gather(dat, key = "year", value = "value", 2:19)
  
  # variable "value" name is file name without .txt extension
  names(dat)[3] <- strsplit(x, "\\.")[[1]][1]
  
  # make year a number
  dat$year <- parse_number(dat$year)
  
  # only keep data between 2004 and 2014
  dat <- dat[-which(dat$year<2004 | dat$year>2014),]
  
  return(dat) 
})

# merge all datafrmaes in the list into one dataframe containing all indicators
governance <- Reduce(function(x,y) merge(x, y, by= c("ISO3", "year")), governance)


## What proportion of Flickr pictures are in PAS ##

# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
# this code was run on the University of Aberdeen 
# High Performance Computer Cluster Maxwell
# on a single node with 64G memory
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #


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

# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
### end of R code run in Maxwell ###
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #

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

# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #
# Visualise proportion of photos in PAs per country ####
# =_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_ #

library(plyr)
library(ggplot2)
library(viridis)
library(rworldmap)

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)


# merge the datasets so every country in the world dataset has the prop of photos in PA
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, photos_PA, by.x = "ADMIN", by.y = "country", all.x = TRUE) 

# convert world map into a format readable by ggplot2
world_df <- fortify(world)

# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

# map
PA_photos <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      prop_photos_in), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis(na.value = "grey")+
  facet_wrap(~year) +
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

## Claculate the total proportion of photos in PA over the years

photos_PA_total <- aggregate(cbind(photos_all, photos_in) ~ country, data = photos_PA, sum)
photos_PA_total$prop <- photos_PA_total$photos_in/photos_PA_total$photos_all

# download a world map at a coarse resolution
world <- getMap(resolution = "coarse")
plot(world)

# merge the datasets so every country in the world dataset has the prop of photos in PA
# all.x = T makes sure that we retain all countries, even those that have no pictures
world@data <- merge (world@data, photos_PA_total, by.x = "ADMIN", by.y = "country", all.x = TRUE) 

# convert world map into a format readable by ggplot2
world_df <- fortify(world)

# merge the fortified dataset and the result of the previous merge
#by.x and by.y are different because column names are different
world_df <- merge(world_df, world@data, by.x = "id", by.y = "ADMIN", all.x = TRUE)  

# map
PA_photos_total <- ggplot() + 
  geom_polygon(data = world_df, aes(x = long, y = lat, group = group, fill =
                                      prop), colour = "black", size = 0.25) +
  coord_fixed() +
  scale_fill_viridis(na.value = "grey")+
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
        plot.title = element_blank(), 
        text=element_text(size=12)) +
  labs(fill = "Proportion of\nFlickr photographs\nin Protected Areas\n")

pdf(file="FlickrPAprop.pdf",width=7.3,height=5)
PA_photos_total
dev.off()

