###################################################
# Script for graphical exploration and analysis
# of SDG indicators and tourism
# Author: Francesca Mancini
# Date created: 2017-12-06
# Date modified: 2018-01-11 
##################################################

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

# delete observations before 2004 and after 2014 (period for which we have flickr data)
mydata <- mydata[-which(mydata$year<2004 | mydata$year>2014),]

# make country_code a factor again
mydata$country_code <- as.factor(mydata$country_code)

# graphical exploration
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
pairs(mydata[,3:12], upper.panel = panel.smooth, lower.panel = panel.cor,
      diag.panel = panel.hist, main = "Tourism") 

pairs(mydata[61:63], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.5")

pairs(mydata[58:60], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.4")

# indicator 8.4.3 has 9 observations only
# we exclude it from the dataset

pairs(mydata[56:57], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.3")

pairs(mydata[51:55], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.2")

pairs(mydata[49:50], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.10")

pairs(mydata[47:48], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 8.1")

pairs(mydata[42:46], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 15.6")

# Target 15.6 has nearly no data
# only one datapoint per country in year 2012
# not comparable with other indicators
# exclude from dataset

pairs(mydata[37:41], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 15.5")

# Indicators 15.5.1y to 15.5.4 have no data
# exclude from dataset


pairs(mydata[35:36], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 15.4")

# Indicator 15.4.4 has no data
# remove from dataset

pairs(mydata[30:34], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 15.2")

pairs(mydata[28:29], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 15.1")

pairs(mydata[24:27], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 14.5 - 14.4")

pairs(mydata[20:23], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 12.4")

# Target 12.4 has no data
# remove from dataset

pairs(mydata[13:19], upper.panel = panel.smooth, lower.panel = panel.cor, 
      diag.panel = panel.hist, main = "Target 12.")

delete <- c("Indicator.8.4.3", "Indicator.15.4.4", "Indicator.12.4.1", "Indicator.12.4.2",
            "Indicator.12.4.3", "Indicator.12.4.4", "Indicator.15.6.1", "Indicator.15.6.2",
            "Indicator.15.6.3", "Indicator.15.6.4", "Indicator.15.6.5", "Indicator.15.5.1.y",
            "Indicator.15.5.2", "Indicator.15.5.3", "Indicator.15.5.4")

mydata <- mydata[, !(names(mydata) %in% delete)] 

deleteagain <-  c("Indicator.14.5.1", "Indicator.15.1.4", "Indicator.8.10.1")
mydata <- mydata[, !(names(mydata) %in% deleteagain)] 


# ===================================================
# Tourism volume and income
# ===================================================
library(nlme)
# only using arrivals_int, trips_dom and exp_int 
# because other variables have too much missing data

domtrips <- lme(log(trips_dom) ~ NFVD_prop, random=~1|country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)
 
summary(domtrips)
plot(domtrips)
qqnorm(domtrips, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(domtrips, type = "normalized"))
pacf(resid(domtrips, type = "normalized"))


arrivals <- lme(log(arrivals_int) ~ NFVD_prop, random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(arrivals)
plot(arrivals)
qqnorm(arrivals, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(arrivals, type = "normalized"))
pacf(resid(arrivals, type = "normalized"))


expend_int <- lme(log(exp_int) ~ NFVD_prop, random=~1|country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(expend_int)
plot(expend_int)
qqnorm(expend_int, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(expend_int, type = "normalized"))
pacf(resid(expend_int, type = "normalized"))



# =======================================================
# Tourism jobs
# =======================================================

employment <- lme(log(employ) ~ NFVD_prop + log(arrivals_int) + log(exp_int),
                  random=~1|country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(employment)
plot(employment)
qqnorm(employment, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(employment, type = "normalized"))
pacf(resid(employment, type = "normalized"))



establishments <- lme(log(establishments) ~ NFVD_prop + log(arrivals_int) + log(exp_int), 
                      random=~1|country_code, data = mydata,
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(establishments)
plot(establishments)
qqnorm(establishments, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(establishments, type = "normalized"))
pacf(resid(establishments, type = "normalized"))


#===========================================
# Goal 8
#===========================================
I.8.1.1 <- lme(Indicator.8.1.1 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.1)
plot(I.8.1.1)
qqnorm(I.8.1.1, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.1.1, type = "normalized"))
pacf(resid(I.8.1.1, type = "normalized"))


I.8.1.2 <- lme(Indicator.8.1.2 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.2)
plot(I.8.1.2)
qqnorm(I.8.1.2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.1.2, type = "normalized"))
pacf(resid(I.8.1.2, type = "normalized"))

I.8.2.1 <- lme(log(Indicator.8.2.1) ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ),
                random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.1)
plot(I.8.2.1)
qqnorm(I.8.2.1, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.1, type = "normalized"))
pacf(resid(I.8.2.1, type = "normalized"))


I.8.2.4 <- lme(Indicator.8.2.4 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.4)
plot(I.8.2.4)
qqnorm(I.8.2.4, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.4, type = "normalized"))
pacf(resid(I.8.2.4, type = "normalized"))


I.8.2.7 <- lme(Indicator.8.2.7 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.7)
plot(I.8.2.7)
qqnorm(I.8.2.7, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.7, type = "normalized"))
pacf(resid(I.8.2.7, type = "normalized"))


I.8.2.7_2 <- lme(Indicator.8.2.7 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
               random=~1|country_code, data = mydata,
               correlation=corARMA(form=~year|country_code, p = 2, q = 0),na.action = na.omit)

AIC(I.8.2.7, I.8.2.7_2)

summary(I.8.2.7_2)
plot(I.8.2.7_2)
qqnorm(I.8.2.7_2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.7_2, type = "normalized"))
pacf(resid(I.8.2.7_2, type = "normalized"))



I.8.2.11 <- lme(Indicator.8.2.11 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.11)
plot(I.8.2.11)
qqnorm(I.8.2.11, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.11, type = "normalized"))
pacf(resid(I.8.2.11, type = "normalized"))


I.8.5.5 <- lme(log(Indicator.8.5.5)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.5.5)
plot(I.8.5.5)
qqnorm(I.8.5.5, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.5.5, type = "normalized"))
pacf(resid(I.8.5.5, type = "normalized"))


I.8.5.11 <- lme(log(Indicator.8.5.11)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.5.11)
plot(I.8.5.11)
qqnorm(I.8.5.11, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.5.11, type = "normalized"))
pacf(resid(I.8.5.11, type = "normalized"))


I.8.5.15 <- lme(log(Indicator.8.5.15)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.5.15)
plot(I.8.5.15)
qqnorm(I.8.5.15, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.5.15, type = "normalized"))
pacf(resid(I.8.5.15, type = "normalized"))


I.8.5.15_2 <- lme(log(Indicator.8.5.15)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                random=~1|country_code, data = mydata,
                correlation=corARMA(form=~year|country_code, p = 3, q = 0),na.action = na.omit)

AIC(I.8.5.15, I.8.5.15_2)
summary(I.8.5.15_2)
plot(I.8.5.15_2)
qqnorm(I.8.5.15_2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.5.15_2, type = "normalized"))
pacf(resid(I.8.5.15_2, type = "normalized"))


I.8.10.10 <- lme(log(Indicator.8.10.10)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.10.10)
plot(I.8.10.10)
qqnorm(I.8.10.10, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.10.10, type = "normalized"))
pacf(resid(I.8.10.10, type = "normalized"))


I.8.10.10_2 <- lme(log(Indicator.8.10.10)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                 random=~1|country_code, data = mydata,
                 correlation=corARMA(form=~year|country_code, p = 4),na.action = na.omit)

AIC(I.8.10.10, I.8.10.10_2)
summary(I.8.10.10_2)
plot(I.8.10.10_2)
qqnorm(I.8.10.10_2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.10.10_2, type = "normalized"))
pacf(resid(I.8.10.10_2, type = "normalized"))


#===========================================
# Goal 12
#===========================================
I.12.2.1 <- lme(Indicator.12.2.1  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                 random=~1|country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.12.2.1)
plot(I.12.2.1)
qqnorm(I.12.2.1, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.12.2.1, type = "normalized"))
pacf(resid(I.12.2.1, type = "normalized"))


I.12.2.2 <- lme(log(Indicator.12.2.2 + 0.001)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
                random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.12.2.2)
plot(I.12.2.2)
qqnorm(I.12.2.2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.12.2.2, type = "normalized"))
pacf(resid(I.12.2.2, type = "normalized"))

