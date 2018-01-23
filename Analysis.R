###################################################
# Script for graphical exploration and analysis
# of SDG indicators and tourism
# Author: Francesca Mancini
# Date created: 2017-12-06
# Date modified: 2018-01-15 
##################################################
# load packages
library(nlme)
library(gridExtra)
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

# add income level
income <- read.csv("Income_level.csv", header = T, colClasses = c("factor", "NULL", "factor"), sep = ",")
mydata <- merge (mydata, income, by = "country_code")

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
# only using arrivals_int, trips_dom and exp_int 
# because other variables have too much missing data

par(mfrow = c(2,2))
hist(log(mydata$trips_dom))
hist(log(mydata$arrivals_int))
hist(log(mydata$exp_int))
hist(mydata$NFVD_prop)

dev.off()

mydata.log <- mydata
mydata.log$trips_dom <- scale(log(mydata.log$trips_dom))
mydata.log$arrivals_int <- scale(log(mydata.log$arrivals_int))
mydata.log$exp_int <- scale(log(mydata.log$exp_int))
mydata.log$NFVD_prop <- scale(mydata.log$NFVD_prop)


domtrips.AR  <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

# domtrips.ARMA <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
#                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
#                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)
# 
# domtrips.AR.var <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
#                          weights = varIdent(form = ~ 1 | income_level),
#                          control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
#                          correlation=corAR1(form=~year|country_code), na.action = na.omit)

# AIC(domtrips.AR, domtrips.ARMA, domtrips.AR.var)
 
summary(domtrips.AR)
grid.arrange(plot(domtrips.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(domtrips.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),             
             qqnorm(domtrips.AR, ~ ranef(.)),
             qqnorm(domtrips.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(domtrips.AR, resType = "normalized"), alpha = .05))



arrivals.AR <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

arrivals.ARMA <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(arrivals.AR, arrivals.ARMA)


grid.arrange(plot(arrivals.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(arrivals.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),             
             qqnorm(arrivals.ARMA, ~ ranef(.)),
             qqnorm(arrivals.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(arrivals.ARMA, resType = "normalized"), alpha = .05))

summary(arrivals.ARMA)

expend_int.AR <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

expend_int.ARMA <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)


AIC(expend_int.AR, expend_int.ARMA)

summary(expend_int.ARMA)

grid.arrange(plot(expend_int.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(expend_int.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),             
             qqnorm(expend_int.ARMA, ~ ranef(.)),
             qqnorm(expend_int.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(expend_int.ARMA, resType = "normalized"), alpha = .05))

# =======================================================
# Tourism jobs
# =======================================================

tourism <- mydata.log[,c("trips_dom", "arrivals_int", "exp_int", "NFVD_prop")]

pairs(tourism, lower.panel = panel.cor, upper.panel = panel.smooth)

source("Collinearity.R")

# calculate VIF
VIF_tourism <- corvif(tourism)
VIF_tourism <- corvif(tourism[,-3])

par(mfrow = c(1,2))
hist(mydata$employ)
hist(mydata$establishments)

par(mfrow = c(1,2))
hist(log(mydata$employ))
hist(log(mydata$establishments))

dev.off()

mydata.log$employ <- scale(log(mydata.log$employ))
mydata.log$establishments <- scale(log(mydata.log$establishments))


employment.1.AR <- lme(employ ~ NFVD_prop + arrivals_int ,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)


employment.2.AR <- lme(employ ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(employment.1.AR)

grid.arrange(plot(employment.1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(employment.1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(employment.1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             qqnorm(employment.1.AR, ~ ranef(.)),
             qqnorm(employment.1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(employment.1.AR, resType = "normalized"), alpha = .05))



summary(employment.2.AR)
grid.arrange(plot(employment.2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(employment.2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(employment.2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(employment.2.AR, ~ ranef(.)),
             qqnorm(employment.2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(employment.2.AR, resType = "normalized"), alpha = .05))



establishments.1.AR <- lme(establishments ~ NFVD_prop + arrivals_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit,
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

establishments.1.ARMA <- lme(establishments ~ NFVD_prop + arrivals_int, 
                               random=~1|country_code, data = mydata.log,
                               correlation=corARMA(form=~year|country_code, p = 3, q = 2),na.action = na.omit,
                              control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

establishments.1.ARMA.2 <- lme(establishments ~ NFVD_prop + arrivals_int, 
                             random=~1|country_code, data = mydata.log,
                             correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit,
                             control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))


AIC(establishments.1.AR, establishments.1.ARMA, establishments.1.ARMA.2)


grid.arrange(plot(establishments.1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(establishments.1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(establishments.1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             qqnorm(establishments.1.ARMA, ~ ranef(.)),
             qqnorm(establishments.1.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(establishments.1.ARMA, resType = "normalized"), alpha = .05))

summary(establishments.1.ARMA)

establishments.2.AR <- lme(establishments ~ NFVD_prop + exp_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)



grid.arrange(plot(establishments.2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(establishments.2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(establishments.2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(establishments.2.AR, ~ ranef(.)),
             qqnorm(establishments.2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(establishments.2.AR, resType = "normalized"), alpha = .05))


summary(establishments.2.AR)


#===========================================
# Goal 8
#===========================================
tourism.all <- mydata.log[,c("trips_dom", "arrivals_int", "exp_int", "NFVD_prop", "employ", "establishments")]

pairs(tourism.all, lower.panel = panel.cor, upper.panel = panel.smooth)

# calculate VIF
VIF_tourism <- corvif(tourism.all)
VIF_tourism <- corvif(tourism.all[,-1])

tourism.sub.1 <- mydata.log[,c("NFVD_prop", "arrivals_int", "establishments")]

pairs(tourism.sub.1, lower.panel = panel.cor, upper.panel = panel.smooth)

# calculate VIF
VIF_tourism.sub.1 <- corvif(tourism.sub.1)


tourism.sub.2 <- mydata.log[,c("NFVD_prop", "exp_int")]

pairs(tourism.sub.2, lower.panel = panel.cor, upper.panel = panel.smooth)

# calculate VIF
VIF_tourism.sub.2 <- corvif(tourism.sub.2)


tourism.sub.3 <- mydata.log[,c("NFVD_prop", "employ")]

pairs(tourism.sub.3, lower.panel = panel.cor, upper.panel = panel.smooth)

# calculate VIF
VIF_tourism.sub.3 <- corvif(tourism.sub.3)



par(mfrow = c(2,5))
hist(mydata.log$Indicator.8.1.1)
hist(mydata.log$Indicator.8.1.2)
hist(mydata.log$Indicator.8.2.1)
hist(mydata.log$Indicator.8.2.4)
hist(mydata.log$Indicator.8.2.7)
hist(mydata.log$Indicator.8.2.11)
hist(mydata.log$Indicator.8.5.5)
hist(mydata.log$Indicator.8.5.11)
hist(mydata.log$Indicator.8.5.15)
hist(mydata.log$Indicator.8.10.10)

summary(mydata.log$Indicator.8.1.1)
summary(mydata.log$Indicator.8.1.2)
summary(mydata.log$Indicator.8.2.1)
summary(mydata.log$Indicator.8.2.4)
summary(mydata.log$Indicator.8.2.7)
summary(mydata.log$Indicator.8.2.11)
summary(mydata.log$Indicator.8.5.5)
summary(mydata.log$Indicator.8.5.11)
summary(mydata.log$Indicator.8.5.15)
summary(mydata.log$Indicator.8.10.10)

par(mfrow = c(2,5))
hist(mydata.log$Indicator.8.1.1)
hist(mydata.log$Indicator.8.1.2)
hist(log(mydata.log$Indicator.8.2.1))
hist(mydata.log$Indicator.8.2.4)
hist(mydata.log$Indicator.8.2.7)
hist(mydata.log$Indicator.8.2.11)
hist(log(mydata.log$Indicator.8.5.5))
hist(log(mydata.log$Indicator.8.5.11))
hist(log(mydata.log$Indicator.8.5.15))
hist(log(mydata.log$Indicator.8.10.10))

dev.off()
mydata.log$Indicator.8.1.1 <- scale(mydata.log$Indicator.8.1.1)
mydata.log$Indicator.8.1.2 <- scale(mydata.log$Indicator.8.1.2)
mydata.log$Indicator.8.2.1 <- scale(log(mydata.log$Indicator.8.2.1))
mydata.log$Indicator.8.2.4 <- scale(mydata.log$Indicator.8.2.4)
mydata.log$Indicator.8.2.7 <- scale(mydata.log$Indicator.8.2.7)
mydata.log$Indicator.8.2.11 <-  scale(mydata.log$Indicator.8.2.11)
mydata.log$Indicator.8.5.5 <- scale(log(mydata.log$Indicator.8.5.5))
mydata.log$Indicator.8.5.11 <- scale(log(mydata.log$Indicator.8.5.11))
mydata.log$Indicator.8.5.15 <- scale(log(mydata.log$Indicator.8.5.15))
mydata.log$Indicator.8.10.10 <- scale(log(mydata.log$Indicator.8.10.10))

#######################
# GDP growth
#######################

I.8.1.1_1.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, 
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.1.1_1.AR, ~ ranef(.)),
             qqnorm(I.8.1.1_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.1.1_1.AR, resType = "normalized"), alpha = .05))


summary(I.8.1.1_1.AR)



I.8.1.1_2.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.1.1_2.AR, ~ ranef(.)),
             qqnorm(I.8.1.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.1.1_2.AR, resType = "normalized"), alpha = .05))


summary(I.8.1.1_2.AR)


I.8.1.1_3.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.1_3.AR.var <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log,
                    control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                    weights = varIdent(form = ~ 1 | income_level),
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.1_3.AR.var.C <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                        random=~1|country_code, data = mydata.log,
                        control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                        weights = varIdent(form = ~ 1 | country_code),
                        correlation=corAR1(form=~year|country_code),na.action = na.omit)


AIC(I.8.1.1_3.AR, I.8.1.1_3.AR.var, I.8.1.1_3.AR.var.C)

grid.arrange(plot(I.8.1.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.1.1_3.AR, ~ ranef(.)),
             qqnorm(I.8.1.1_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.1.1_3.AR, resType = "normalized"), alpha = .05))

plot(I.8.1.1_3.AR.var.C, form = resid(., type = "n") ~ fitted(.)|country_code, abline = 0)

summary(I.8.1.1_3.AR.var.C)

# simple model shows residual variance increasing with fitted values
# variance structure by income level does not help
# variance structure by country fixes that but calculates variances for 1 or 2 datapoints!

##########################
# GDP growth (per capita)
##########################

I.8.1.2_1.AR <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log, 
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.1.2_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.2_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.2_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.1.2_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.1.2_1.AR, ~ ranef(.)),
             qqnorm(I.8.1.2_1.AR, ~ resid(.), abline = c(0, 1)),
             plot(ACF(I.8.1.2_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.1.2_1.AR)


I.8.1.2_2.AR <- lme(Indicator.8.1.2 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.2_2.ARMA <- lme(Indicator.8.1.2 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


grid.arrange(plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.1.2_2.AR, ~ ranef(.)),
             qqnorm(I.8.1.2_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.1.2_2.AR, resType = "normalized"), alpha = .05))

summary(I.8.1.2_2.AR)


I.8.1.2_3.AR <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.2_3.var <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.2_3.var.C <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)



AIC(I.8.1.2_3.AR, I.8.1.2_3.var, I.8.1.2_3.var.C)

grid.arrange(plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.1.2_3.AR, ~ ranef(.)),
             qqnorm(I.8.1.2_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.1.2_3.AR, resType = "normalized"), alpha = .05))

plot(I.8.1.1_3.AR.var.C, form = resid(., type = "n") ~ fitted(.)|country_code, abline = 0)

summary(I.8.1.2_3.ARMA)

# again unequal variance by income_level doesn't help much
# but by country_code estimates variances from 1 or 2 datapoints

###############################
# Employment in agriculture
###############################

I.8.2.1_1.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.1_1.ARMA <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(I.8.2.1_1.AR, I.8.2.1_1.ARMA)

grid.arrange(plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.1_1.ARMA, ~ ranef(.)),
             qqnorm(I.8.2.1_1.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.1_1.ARMA, resType = "normalized"), alpha = .05))

summary(I.8.2.1_1.ARMA)


I.8.2.1_2.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.2.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.1_2.AR, ~ ranef(.)),
             qqnorm(I.8.2.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.1_2.AR)


I.8.2.1_3.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.2.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.1_3.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.1_3.AR, ~ ranef(.)),
             qqnorm(I.8.2.1_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.1_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.1_3.AR)


###################################
# Employment in industry
###################################


I.8.2.4_1.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.2.4_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.4_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.4_1.AR, ~ ranef(.)),
             qqnorm(I.8.2.4_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.4_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.4_1.AR)


I.8.2.4_2.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.2.4_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.4_2.AR, ~ ranef(.)),
             qqnorm(I.8.2.4_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.4_2.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.4_2.AR)


I.8.2.4_3.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.2.4_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.2.4_3.AR, ~ ranef(.)),
             qqnorm(I.8.2.4_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.4_3.AR, resType = "normalized"), alpha = .05))

# outlier
plot(I.8.2.4_3.AR, form = resid(., type = "n") ~ fitted(.)|country_code, abline = 0)

summary(I.8.2.4_3.AR)

###############################
# Employment in services
###############################

I.8.2.7_1.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.2.7_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.7_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.7_1.AR, ~ ranef(.)),
             qqnorm(I.8.2.7_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.7_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.7_1.AR)

I.8.2.7_2.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.7_2.ARMA <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(I.8.2.7_2.AR, I.8.2.7_2.ARMA)

grid.arrange(plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.7_2.ARMA, ~ ranef(.)),
             qqnorm(I.8.2.7_2.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.7_2.ARMA, resType = "normalized"), alpha = .05))

summary(I.8.2.7_2.ARMA)


I.8.2.7_3.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.2.7_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_3.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.7_3.AR, ~ ranef(.)),
             qqnorm(I.8.2.7_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.7_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.7_3.AR)

###############################
# GNI growth
###############################

I.8.2.11_1.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_1.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.2.11_1.var <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                        control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                        correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_1.var.C <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                        control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                        correlation=corAR1(form=~year|country_code),na.action = na.omit)


AIC(I.8.2.11_1.AR, I.8.2.11_1.ARMA, I.8.2.11_1.var, I.8.2.11_1.var.C)

grid.arrange(plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.11_1.AR, ~ ranef(.)),
             qqnorm(I.8.2.11_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.11_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.11_1.var.C)



I.8.2.11_2.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.2.11_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.11_2.AR, ~ ranef(.)),
             qqnorm(I.8.2.11_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.11_2.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.11_2.AR)


I.8.2.11_3.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_3.var <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

# I.8.2.11_3.AR.var <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
#                          random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | country_code),
#                          control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
#                          correlation=corAR1(form=~year|country_code),na.action = na.omit)


I.8.2.11_3.var.C <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                           random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)

AIC(I.8.2.11_3.AR, I.8.2.11_3.var, I.8.2.11_3.var.C)

grid.arrange(plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.2.11_3.AR, ~ ranef(.)),
             qqnorm(I.8.2.11_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.2.11_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.2.11_3.var.C)


################################
# Unemployment
################################

# Total unemployment

I.8.5.5_1.AR <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.5_1.AR, ~ ranef(.)),
             qqnorm(I.8.5.5_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.5_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.5.5_1.AR)



I.8.5.5_2.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_2.ARMA <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,
                    control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.8.5.5_2.AR, I.8.5.5_2.ARMA)

grid.arrange(plot(I.8.5.5_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.5_2.ARMA, ~ ranef(.)),
             qqnorm(I.8.5.5_2.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.5_2.ARMA, resType = "normalized"), alpha = .05))

summary(I.8.5.5_2.ARMA)



I.8.5.5_3.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.5_3.AR, ~ ranef(.)),
             qqnorm(I.8.5.5_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.5_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.5.5_3.AR)

# Unemployment youth

I.8.5.11_1.AR <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.11_1.AR, ~ ranef(.)),
             qqnorm(I.8.5.11_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.11_1.AR, resType = "normalized"), alpha = .05))

summary(I.8.5.11_1.AR)


I.8.5.11_2.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.11_2.AR, ~ ranef(.)),
             qqnorm(I.8.5.11_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.11_2.AR, resType = "normalized"), alpha = .05))

summary(I.8.5.11_2.AR)



I.8.5.11_3.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.11_3.AR, ~ ranef(.)),
             qqnorm(I.8.5.11_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.11_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.5.11_3.AR)


# Waged and salaried workers

I.8.5.15_1.AR <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_1.ARMA <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_1.var <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_1.ARMA.var.C <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.8.5.15_1.AR, I.8.5.15_1.ARMA, I.8.5.15_1.var)

grid.arrange(plot(I.8.5.15_1.var, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_1.var, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.15_1.var, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.15_1.var, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.15_1.var, ~ ranef(.)),
             qqnorm(I.8.5.15_1.var, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.15_1.var, resType = "normalized"), alpha = .05))

summary(I.8.5.15_1.var)


I.8.5.15_2.AR <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_2.ARMA <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_2.ARMA.var <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(I.8.5.15_2.AR, I.8.5.15_2.ARMA, I.8.5.15_2.ARMA.var)

grid.arrange(plot(I.8.5.15_2.ARMA.var, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_2.ARMA.var, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.15_2.ARMA.var, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.15_2.ARMA.var, ~ ranef(.)),
             qqnorm(I.8.5.15_2.ARMA.var, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.15_2.ARMA.var, resType = "normalized"), alpha = .05))

summary(I.8.5.15_2.ARMA.var)

I.8.5.15_3.AR <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_3.ARMA <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_3.ARMA.var <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.8.5.15_3.AR, I.8.5.15_3.ARMA, I.8.5.15_3.ARMA.var)

grid.arrange(plot(I.8.5.15_3.ARMA.var, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_3.ARMA.var, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.15_3.ARMA.var, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.15_3.ARMA.var, ~ ranef(.)),
             qqnorm(I.8.5.15_3.ARMA.var, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.5.15_3.ARMA.var, resType = "normalized"), alpha = .05))

summary(I.8.5.15_3.ARMA.var)

#################################
# Access to banking
#################################

I.8.10.10_1.AR <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.10.10_1.ARMA <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.8.10.10_1.AR, I.8.10.10_1.ARMA)

grid.arrange(plot(I.8.10.10_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.10.10_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.10.10_1.ARMA, ~ ranef(.)),
             qqnorm(I.8.10.10_1.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.10.10_1.ARMA, resType = "normalized"), alpha = .05))

summary(I.8.10.10_1.ARMA)


I.8.10.10_2.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.10.10_2.ARMA <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(I.8.10.10_2.AR, I.8.10.10_2.ARMA)

grid.arrange(plot(I.8.10.10_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.10.10_2.ARMA, ~ ranef(.)),
             qqnorm(I.8.10.10_2.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.10.10_2.ARMA, resType = "normalized"), alpha = .05))

summary(I.8.10.10_2.ARMA)


I.8.10.10_3.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.10.10_3.AR, ~ ranef(.)),
             qqnorm(I.8.10.10_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.8.10.10_3.AR, resType = "normalized"), alpha = .05))

summary(I.8.10.10_3.AR)


#===========================================
# Goal 12
#===========================================

par(mfrow = c(2,4))
hist(mydata.log$Indicator.12.2.1)
hist(mydata.log$Indicator.12.2.2)
hist(mydata.log$Indicator.12.2.3)
hist(mydata.log$Indicator.12.2.4)
hist(mydata.log$Indicator.12.2.5)
hist(mydata.log$Indicator.12.2.6)
hist(mydata.log$Indicator.12.2.7)


summary(mydata.log$Indicator.12.2.1)
summary(mydata.log$Indicator.12.2.2)
summary(mydata.log$Indicator.12.2.3)
summary(mydata.log$Indicator.12.2.4)
summary(mydata.log$Indicator.12.2.5)
summary(mydata.log$Indicator.12.2.6)
summary(mydata.log$Indicator.12.2.7)

mydata.log$Indicator.12.2.1 <- scale(mydata.log$Indicator.12.2.1)

###########################################################
# Adjusted net savings, excl particulate emission damage
###########################################################

I.12.2.1_1.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.12.2.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.12.2.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.12.2.1_1.AR, ~ ranef(.)),
             qqnorm(I.12.2.1_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.12.2.1_1.AR, resType = "normalized"), alpha = .05))

summary(I.12.2.1_1.AR)



I.12.2.1_2.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.12.2.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.12.2.1_2.AR, ~ ranef(.)),
             qqnorm(I.12.2.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.12.2.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.12.2.1_2.AR)


I.12.2.1_3.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.12.2.1_3.var <- lme(Indicator.12.2.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                  correlation=corAR1(form=~year|country_code),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.12.2.1_3.AR, I.12.2.1_3.var)

grid.arrange(plot(I.12.2.1_3.var, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_3.var, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_3.var, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.12.2.1_3.var, ~ ranef(.)),
             qqnorm(I.12.2.1_3.var, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.12.2.1_3.var, resType = "normalized"), alpha = .05))

summary(I.12.2.1_3.var)



#===========================================
# Goal 14
#===========================================

par(mfrow = c(2,2))
hist(log(mydata.log$Indicator.14.4.1 + 1))
hist(log(mydata.log$Indicator.14.4.2 + 1))
hist(log(mydata.log$Indicator.14.4.3 + 1))
#hist(log(mydata.log$Indicator.14.5.1 + 1))



summary(mydata.log$Indicator.14.4.1)
summary(mydata.log$Indicator.14.4.2)
summary(mydata.log$Indicator.14.4.3)
#summary(mydata.log$Indicator.14.5.1)


mydata.log$Indicator.14.4.1 <- scale(log(mydata.log$Indicator.14.4.1 + 1))
mydata.log$Indicator.14.4.2 <- scale(log(mydata.log$Indicator.14.4.2 + 1))
mydata.log$Indicator.14.4.3 <- scale(log(mydata.log$Indicator.14.4.3 + 1))

######################################
# Aquaculture production
######################################

I.14.4.1_1.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.1_1.var <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       correlation=corAR1(form=~year|country_code), na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.14.4.1_1.var.C <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                      correlation=corAR1(form=~year|country_code), na.action = na.omit,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.14.4.1_1.AR, I.14.4.1_1.var)

grid.arrange(plot(I.14.4.1_1.var, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.1_1.var, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.1_1.var, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.1_1.var, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.1_1.var, ~ ranef(.)),
             qqnorm(I.14.4.1_1.var, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.1_1.var, resType = "normalized"), alpha = .05))

summary(I.14.4.1_1.var)




I.14.4.1_2.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)



grid.arrange(plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.1_2.AR, ~ ranef(.)),
             qqnorm(I.14.4.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.1_2.AR)




I.14.4.1_3.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.1_3.AR, ~ ranef(.)),
             qqnorm(I.14.4.1_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.1_3.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.1_3.AR)

##########################
# Capture fisheries
##########################

I.14.4.2_1.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.2_1.AR, ~ ranef(.)),
             qqnorm(I.14.4.2_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.2_1.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.2_1.AR)


I.14.4.2_2.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.2_2.AR, ~ ranef(.)),
             qqnorm(I.14.4.2_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.2_2.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.2_2.AR)


I.14.4.2_3.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.14.4.2_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.2_3.AR, ~ ranef(.)),
             qqnorm(I.14.4.2_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.2_3.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.2_3.AR)


#######################################
# Total fisheries production
#######################################

I.14.4.3_1.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.14.4.3_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.3_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.3_1.AR, ~ ranef(.)),
             qqnorm(I.14.4.3_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.3_1.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.3_1.AR)

I.14.4.3_2.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.3_2.AR, ~ ranef(.)),
             qqnorm(I.14.4.3_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.3_2.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.3_2.AR)


I.14.4.3_3.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.14.4.3_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.3_3.AR, ~ ranef(.)),
             qqnorm(I.14.4.3_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.14.4.3_3.AR, resType = "normalized"), alpha = .05))

summary(I.14.4.3_3.AR)


#===========================================
# Goal 15
#===========================================

par(mfrow = c(1,2))
hist(mydata.log$Indicator.15.1.1)
hist(mydata.log$Indicator.15.4.1)


summary(mydata.log$Indicator.15.1.1)
summary(mydata.log$Indicator.15.4.1)


mydata.log$Indicator.15.1.1 <- scale(mydata.log$Indicator.15.1.1)
mydata.log$Indicator.15.4.1 <- scale(mydata.log$Indicator.15.4.1)

#######################################
# Forest area
#######################################

I.15.1.1_1.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.15.1.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.1.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.1.1_1.AR, ~ ranef(.)),
             qqnorm(I.15.1.1_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.1.1_1.AR, resType = "normalized"), alpha = .05))

summary(I.15.1.1_1.AR)



I.15.1.1_2.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

grid.arrange(plot(I.15.1.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.1.1_2.AR, ~ ranef(.)),
             qqnorm(I.15.1.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.1.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.15.1.1_2.AR)


I.15.1.1_3.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.1.1_3.ARMA <- lme(Indicator.15.1.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.15.1.1_3.AR, I.15.1.1_3.ARMA)

grid.arrange(plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.1.1_3.ARMA, ~ ranef(.)),
             qqnorm(I.15.1.1_3.ARMA, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.1.1_3.ARMA, resType = "normalized"), alpha = .05))

summary(I.15.1.1_3.ARMA)


########################################
# Coverage by PA of important sites 
# for mountain biodiversity
########################################

I.15.4.1_1.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.15.4.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.4.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.4.1_1.AR, ~ ranef(.)),
             qqnorm(I.15.4.1_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.4.1_1.AR, resType = "normalized"), alpha = .05))

summary(I.15.4.1_1.AR)


I.15.4.1_2.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

grid.arrange(plot(I.15.4.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.4.1_2.AR, ~ ranef(.)),
             qqnorm(I.15.4.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.4.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.15.4.1_2.AR)




I.15.4.1_3.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.4.1_3.AR, ~ ranef(.)),
             qqnorm(I.15.4.1_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.4.1_3.AR, resType = "normalized"), alpha = .05))

summary(I.15.4.1_3.AR)


#######################################
# Red List Index
#######################################
summary(mydata.log$Indicator.15.5.1.x)
hist(mydata.log$Indicator.15.5.1.x)

I.15.5.1_1.AR <- lme(Indicator.15.5.1.x  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.5.1_1.AR, ~ ranef(.)),
             qqnorm(I.15.5.1_1.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.5.1_1.AR, resType = "normalized"), alpha = .05))

summary(I.15.5.1_1.AR)


I.15.5.1_2.AR <- lme(Indicator.15.5.1.x  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

grid.arrange(plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.5.1_2.AR, ~ ranef(.)),
             qqnorm(I.15.5.1_2.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.5.1_2.AR, resType = "normalized"), alpha = .05))

summary(I.15.5.1_2.AR)




I.15.5.1_3.AR <- lme(Indicator.15.5.1.x  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.5.1_3.AR, ~ ranef(.)),
             qqnorm(I.15.5.1_3.AR, ~ resid(., type = "n"), abline = c(0, 1)),
             plot(ACF(I.15.5.1_3.AR, resType = "normalized"), alpha = .05))

summary(I.15.5.1_3.AR)


