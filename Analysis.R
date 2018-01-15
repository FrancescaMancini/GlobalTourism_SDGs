###################################################
# Script for graphical exploration and analysis
# of SDG indicators and tourism
# Author: Francesca Mancini
# Date created: 2017-12-06
# Date modified: 2018-01-15 
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
library(nlme)
library(gridExtra)
# only using arrivals_int, trips_dom and exp_int 
# because other variables have too much missing data

par(mfrow = c(2,2))
hist(log(mydata$trips_dom))
hist(log(mydata$arrivals_int))
hist(log(mydata$exp_int))
hist(mydata$NFVD_prop)

dev.off()

mydata.log <- mydata
mydata.log$trips_dom <- log(mydata.log$trips_dom)
mydata.log$arrivals_int <- log(mydata.log$arrivals_int)
mydata.log$exp_int <- log(mydata.log$exp_int)


domtrips.AR  <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

domtrips.ARMA <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

domtrips.AR.var <- lme(trips_dom ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                         weights = varIdent(form = ~ 1 | income_level),
                         control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                         correlation=corAR1(form=~year|country_code), na.action = na.omit)

AIC(domtrips.AR, domtrips.ARMA, domtrips.AR.var)
 
summary(domtrips.AR.var)
plot(domtrips.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
plot(domtrips.AR.var, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0)
plot(domtrips.AR.var, form = resid(., type = "n") ~ fitted(.) | country_code, abline = 0) # country improves a lot

qqnorm(domtrips.AR.var, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(domtrips.AR.var, type = "normalized"), lag.max = 10)
pacf(resid(domtrips.AR.var, type = "normalized"), lag.max = 10)


arrivals.AR <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

arrivals.ARMA <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

arrivals.AR.var <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                         weights = varIdent(form = ~ 1 | income_level),
                         control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corAR1(form=~year|country_code), na.action = na.omit)



AIC(arrivals.AR, arrivals.ARMA, arrivals.AR.var)


summary(arrivals.AR.var)
plot(arrivals.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
plot(arrivals.AR, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0)

qqnorm(arrivals.AR.var, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(arrivals.AR.var, type = "normalized"), lag.max = 10)
pacf(resid(arrivals.AR.var, type = "normalized"), lag.max = 10)


expend_int.AR <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

expend_int.ARMA <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)

expend_int.AR.var <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                         weights = varIdent(form = ~ 1 | income_level),
                         control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                         correlation = corAR1(form=~year|country_code),na.action = na.omit)


AIC(expend_int.AR, expend_int.ARMA, expend_int.AR.var, expend_int.AR.var.C)

summary(expend_int.AR.var)
plot(expend_int.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
plot(expend_int.AR.var, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0)
qqnorm(expend_int.AR.var, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(expend_int.AR.var, type = "normalized"), lag.max = 10)
pacf(resid(expend_int.AR.var, type = "normalized"), lag.max = 10)



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

mydata.log$employ <- log(mydata.log$employ)
mydata.log$establishments <- log(mydata.log$establishments)


employment.1.AR <- lme(employ ~ NFVD_prop + arrivals_int + trips_dom,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

employment.1.AR.var <- lme(employ ~ NFVD_prop + arrivals_int + trips_dom,
                           random=~1|country_code, data = mydata.log,
                           weights = varIdent(form = ~ 1 | income_level),
                           #control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)


employment.2.AR <- lme(employ ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

employment.2.AR.var <- lme(employ ~ NFVD_prop + exp_int,
                           random=~1|country_code, data = mydata.log,
                           weights = varIdent(form = ~ 1 | income_level),
                           #control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)

AIC(employment.1.AR, employment.1.AR.var)

summary(employment.1.AR.var)
plot(employment.1.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(employment.1.AR.var, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(employment.1.AR.var, type = "normalized"), lag.max = 10)
pacf(resid(employment.1.AR.var, type = "normalized"), lag.max = 10)

AIC(employment.2.AR, employment.2.AR.var)

summary(employment.2)
plot(employment.2.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(employment.2.AR.var, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(employment.2.AR.var, type = "normalized"), lag.max = 10)
pacf(resid(employment.2.AR.var, type = "normalized"), lag.max = 10)



establishments.1.AR <- lme(establishments ~ NFVD_prop + arrivals_int + trips_dom, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)

establishments.1.AR.var <- lme(establishments ~ NFVD_prop + arrivals_int + trips_dom, 
                               random=~1|country_code, data = mydata.log,
                               weights = varIdent(form = ~ 1 | income_level),
                               correlation=corAR1(form=~year|country_code),na.action = na.omit)


establishments.2.AR <- lme(establishments ~ NFVD_prop + exp_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)

establishments.2.AR.var <- lme(establishments ~ NFVD_prop + exp_int, 
                               random=~1|country_code, data = mydata.log,
                               weights = varIdent(form = ~ 1 | income_level),
                               correlation=corAR1(form=~year|country_code),na.action = na.omit)

establishments.2.AR.var.C <- lme(establishments ~ NFVD_prop + exp_int, 
                               random=~1|country_code, data = mydata.log,
                               control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                               weights = varIdent(form = ~ 1 | country_code),
                               correlation=corAR1(form=~year|country_code),na.action = na.omit)


AIC(establishments.1.AR, establishments.1.AR.var)

summary(establishments.1.AR.var)
plot(establishments.1.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(establishments.1.AR.var, ~ resid(., type = "p"), abline = c(0, 1))
qqnorm(establishments.1.AR.var, ~ranef(., level=1), abline = c(0, 1))
acf(resid(establishments.1.AR.var, type = "normalized"))
pacf(resid(establishments.1.AR.var, type = "normalized"))

AIC(establishments.2.AR, establishments.2.AR.var, establishments.2.AR.var.C)

summary(establishments.2.AR.var)

res1 <- plot(establishments.2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "AR1 - Equal variance")
res2 <- plot(establishments.2.AR.var, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "AR1 - Unequal variance (income level)")
res3 <- plot(establishments.2.AR.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "AR1 - Unequal variance (country)")
grid.arrange(res1, res2, res3)

q1 <- qqnorm(establishments.2.AR, ~ resid(., type = "n"), abline = c(0, 1),
             main = "AR1 - Equal variance")
q2 <- qqnorm(establishments.2.AR.var, ~ resid(., type = "n"), abline = c(0, 1),
             main = "AR1 - Unequal variance (income level)")
q3 <- qqnorm(establishments.2.AR.var.C, ~ resid(., type = "n"), abline = c(0, 1),
             main = "AR1 - Unequal variance (country)")
grid.arrange(q1, q2, q3)


plot(establishments.2.AR, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0,
     main = "AR1 - Equal variance (by income level)")
plot(establishments.2.AR, form = resid(., type = "n") ~ fitted(.) | country_code, abline = 0,
     main = "AR1 - Equal variance (by country)") # country improves a lot


par(mfrow = c(3,1))
acf(resid(establishments.2.AR, type = "normalized"))
acf(resid(establishments.2.AR.var, type = "normalized"))
acf(resid(establishments.2.AR.var.C, type = "normalized"))


par(mfrow = c(3,1))
pacf(resid(establishments.2.AR, type = "normalized"))
pacf(resid(establishments.2.AR.var, type = "normalized"))
pacf(resid(establishments.2.AR.var.C, type = "normalized"))

dev.off()

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

mydata.log$Indicator.8.2.1 <- log(mydata.log$Indicator.8.2.1)
mydata.log$Indicator.8.5.5 <- log(mydata.log$Indicator.8.5.5)
mydata.log$Indicator.8.5.11 <- log(mydata.log$Indicator.8.5.11)
mydata.log$Indicator.8.5.15 <- log(mydata.log$Indicator.8.5.15)
mydata.log$Indicator.8.10.10 <- log(mydata.log$Indicator.8.10.10)



I.8.1.1_1 <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log, 
               correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)

I.8.1.1_1.var <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log, 
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | country_code),
                 correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)


I.8.1.1_2 <- lme(Indicator.8.1.1 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)

I.8.1.1_3 <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)

I.8.1.1_3.var <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)

I.8.1.1_3.var.C <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corARMA(form=~year|country_code, p = 4, q = 1),na.action = na.omit)


AIC(I.8.1.1_1, I.8.1.1_1.var)

summary(I.8.1.1_1)
plot(I.8.1.1_1, form = resid(., type = "n") ~ fitted(.), abline = 0)
plot(I.8.1.1_1, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0)
qqnorm(I.8.1.1_1, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.1.1_1, type = "normalized"), lag.max = 10)
pacf(resid(I.8.1.1_1, type = "normalized"), lag.max = 10)


summary(I.8.1.1_2)
plot(I.8.1.1_2, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.1.1_2, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.1.1_2, type = "normalized"))
pacf(resid(I.8.1.1_2, type = "normalized"))

AIC(I.8.1.1_3, I.8.1.1_3.var, I.8.1.1_3.var.C)

summary(I.8.1.1_3)
plot(I.8.1.1_3, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.1.1_3.var.C, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.1.1_3.var.C, type = "normalized"))
pacf(resid(I.8.1.1_3.var.C, type = "normalized"))


I.8.1.2_1 <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log, 
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.1.2_1.var <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log, 
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.1.2_1.var.C <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, 
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



I.8.1.2_2 <- lme(Indicator.8.1.2 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.1.2_3 <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.1.2_3.var <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.1.2_3.var.C <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



AIC(I.8.1.2_1, I.8.1.2_1.var, I.8.1.2_1.var.C)

summary(I.8.1.2_1)
plot(I.8.1.2_1, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.1.2_1.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.1.2_1.var.C, type = "normalized"))
pacf(resid(I.8.1.2_1.var.C, type = "normalized"))

summary(I.8.1.2_2)
plot(I.8.1.2_2, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.1.2_2, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.1.2_2, type = "normalized"))
pacf(resid(I.8.1.2_2, type = "normalized"))

AIC(I.8.1.2_3, I.8.1.2_3.var, I.8.1.2_3.var.C)

summary(I.8.1.2_3.var.C)
plot(I.8.1.2_3, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.1.2_3.var.C, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.1.2_3.var.C, type = "normalized"))
pacf(resid(I.8.1.2_3.var.C, type = "normalized"))


res1 <- plot(I.8.1.2_3, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "ARMA - Equal variance")
res2 <- plot(I.8.1.2_3.var, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "ARMA - Unequal variance (income level)")
res3 <- plot(I.8.1.2_3.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0,
             main = "ARMA - Unequal variance (country)")
grid.arrange(res1, res2, res3)

q1 <- qqnorm(I.8.1.2_3, ~ resid(., type = "n"), abline = c(0, 1),
             main = "ARMA - Equal variance")
q2 <- qqnorm(I.8.1.2_3.var, ~ resid(., type = "n"), abline = c(0, 1),
             main = "ARMA - Unequal variance (income level)")
q3 <- qqnorm(I.8.1.2_3.var.C, ~ resid(., type = "n"), abline = c(0, 1),
             main = "ARMA - Unequal variance (country)")
grid.arrange(q1, q2, q3)


plot(I.8.1.2_3, form = resid(., type = "n") ~ fitted(.)|income_level, abline = 0,
     main = "AR1 - Equal variance (by income level)")
plot(I.8.1.2_3, form = resid(., type = "n") ~ fitted(.) | country_code, abline = 0,
     main = "AR1 - Equal variance (by country)") # country improves a lot
plot(I.8.1.2_3.var, form = resid(., type = "n") ~ fitted(.) | country_code, abline = 0,
     main = "AR1 - Unequal variance (income level)") # country improves a lot



I.8.2.1_1.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.1_1.ARMA <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.1_1.ARMA.var <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                      weights = varIdent(form = ~ 1 | income_level),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.1_1.ARMA.var.C <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                          random=~1|country_code, data = mydata.log,
                          control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                          weights = varIdent(form = ~ 1 | country_code),
                          correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



I.8.2.1_2 <- lme(Indicator.8.2.1 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.1_3 <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

AIC(I.8.2.1_1.AR, I.8.2.1_1.ARMA, I.8.2.1_1.ARMA.var, I.8.2.1_1.ARMA.var.C)

summary(I.8.2.1_1.ARMA)
plot(I.8.2.1_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.1_1.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.1_1.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.2.1_1.ARMA.var.C, type = "normalized"))


summary(I.8.2.1_2)
plot(I.8.2.1_2, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.1_2, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.1_2, type = "normalized"))
pacf(resid(I.8.2.1_2, type = "normalized"))


summary(I.8.2.1_3)
plot(I.8.2.1_3, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.1_3, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.1_3, type = "normalized"))
pacf(resid(I.8.2.1_2, type = "normalized"))


I.8.2.4_1 <- lme(Indicator.8.2.4 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.4_2 <- lme(Indicator.8.2.4 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.4_3 <- lme(Indicator.8.2.4 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

summary(I.8.2.4_1)
plot(I.8.2.4_1, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.4_1, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.4_1, type = "normalized"))
pacf(resid(I.8.2.4_1, type = "normalized"))


summary(I.8.2.4_2)
plot(I.8.2.4_2, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.4_2, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.4_2, type = "normalized"))
pacf(resid(I.8.2.4_2, type = "normalized"))


summary(I.8.2.4_3)
plot(I.8.2.4_3, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.4_3, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.4_3, type = "normalized"))
pacf(resid(I.8.2.4_3, type = "normalized"))


I.8.2.7_1 <- lme(Indicator.8.2.7 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
               correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.7_2 <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.7_2.var <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log, 
                 control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.7_2.var.C <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, 
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.2.7_3 <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.7_3.var <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log, 
                 control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                 weights = varIdent(form = ~ 1 | income_level),
                 correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.7_3.var.C <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, 
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     weights = varIdent(form = ~ 1 | country_code),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


summary(I.8.2.7_1)
plot(I.8.2.7_1, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.7_1, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.7_1, type = "normalized"))
pacf(resid(I.8.2.7_1, type = "normalized"))

AIC(I.8.2.7_2, I.8.2.7_2.var, I.8.2.7_2.var.C)

summary(I.8.2.7_2.var.C)
plot(I.8.2.7_2.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.7_2.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.7_2.var.C, type = "normalized"))
pacf(resid(I.8.2.7_2.var.C, type = "normalized"))

AIC(I.8.2.7_3, I.8.2.7_3.var, I.8.2.7_3.var.C)

summary(I.8.2.7_3.var.C)
plot(I.8.2.7_3.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.7_3.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.7_3.var.C, type = "normalized"))
pacf(resid(I.8.2.7_3.var.C, type = "normalized"))


# I.8.2.7_2 <- lme(Indicator.8.2.7 ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
#                random=~1|country_code, data = mydata,
#                correlation=corARMA(form=~year|country_code, p = 2, q = 0),na.action = na.omit)
# 
# AIC(I.8.2.7, I.8.2.7_2)
# 
# summary(I.8.2.7_2)
# plot(I.8.2.7_2)
# qqnorm(I.8.2.7_2, ~ resid(., type = "p"), abline = c(0, 1))
# acf(resid(I.8.2.7_2, type = "normalized"))
# pacf(resid(I.8.2.7_2, type = "normalized"))
# 

I.8.2.11_1 <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_1.var <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                        control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                        correlation=corAR1(form=~year|country_code),na.action = na.omit)


I.8.2.11_1.var.C <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_2.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_2.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.11_2.ARMA.var <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.11_2.ARMA.var.C <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                           random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.2.11_3.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_3.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.2.11_3.ARMA.var <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

# I.8.2.11_3.AR.var <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
#                          random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | country_code),
#                          control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
#                          correlation=corAR1(form=~year|country_code),na.action = na.omit)


I.8.2.11_3.ARMA.var.C <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                           random=~1|country_code, data = mydata.log,weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



AIC(I.8.2.11_1, I.8.2.11_1.var, I.8.2.11_1.var.C)

summary(I.8.2.11_1.var.C)
plot(I.8.2.11_1.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.11_1.var.C, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.11_1.var.C, type = "normalized"))
pacf(resid(I.8.2.11_1.var.C, type = "normalized"))

AIC(I.8.2.11_2.AR, I.8.2.11_2.ARMA, I.8.2.11_2.ARMA.var, I.8.2.11_2.ARMA.var.C)

summary(I.8.2.11_2.ARMA.var.C)
plot(I.8.2.11_2.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.2.11_2.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.2.11_2.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.2.11_2.ARMA.var.C, type = "normalized"))

AIC(I.8.2.11_3.AR, I.8.2.11_3.ARMA, I.8.2.11_3.ARMA.var, I.8.2.11_3.ARMA.var.C)

summary(I.8.2.11_3.ARMA.var.C)
plot(I.8.2.11_3.ARMA.var.C)
qqnorm(I.8.2.11_3.ARMA.var.C, ~ resid(., type = "p"), abline = c(0, 1))
acf(resid(I.8.2.11_3.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.2.11_3.ARMA.var.C, type = "normalized"))

I.8.5.5_1.AR <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_1.ARMA <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log,
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.5_1.ARMA.var <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
                            random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                            control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                            correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.5.5_1.ARMA.var.C <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                      control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.5.5_2.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_2.ARMA <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,
                    control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.5_2.AR.var <- lme(Indicator.8.5.5  ~ NFVD_prop + exp_int,
                          random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                          control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000),
                          correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_2.AR.var.C <- lme(Indicator.8.5.5  ~ NFVD_prop + exp_int,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000),
                        correlation=corAR1(form=~year|country_code),na.action = na.omit)



I.8.5.5_3.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_3.ARMA <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log,
                    control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                    correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.5_3.AR.var <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                      control = list(maxIter = 100000, msMaxIter = 100000, niterEM = 100000,  msMaxEval = 1000),
                      correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)

I.8.5.5_3.AR.var.C <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                        control = list(maxIter = 100000, msMaxIter = 100000, niterEM = 100000,  msMaxEval = 1000),
                        correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)



AIC(I.8.5.5_1.AR, I.8.5.5_1.ARMA, I.8.5.5_1.ARMA.var, I.8.5.5_1.ARMA.var.C)

summary(I.8.5.5_1.ARMA.var.C)
plot(I.8.5.5_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.5.5_1.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.5_1.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.5.5_1.ARMA.var.C, type = "normalized"))


AIC(I.8.5.5_2.AR, I.8.5.5_2.ARMA, I.8.5.5_2.AR.var, I.8.5.5_2.AR.var.C)

summary(I.8.5.5_2.AR.var.C)
plot(I.8.5.5_2.AR.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)
qqnorm(I.8.5.5_2.AR.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.5_2.AR.var.C, type = "normalized"))
pacf(resid(I.8.5.5_2.AR.var.C, type = "normalized"))

AIC(I.8.5.5_3.AR, I.8.5.5_3.ARMA, I.8.5.5_3.AR.var, I.8.5.5_3.AR.var.C)

summary(I.8.5.5_3.AR.var.C)
plot(I.8.5.5_3.AR.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)   # income_level ok
qqnorm(I.8.5.5_3.AR.var.C, ~ resid(., type = "n"), abline = c(0, 1))            # normality improves considerably with country
acf(resid(I.8.5.5_3.AR.var.C, type = "normalized"))
pacf(resid(I.8.5.5_3.AR.var.C, type = "normalized"))


I.8.5.11_1.AR <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.11_1.ARMA <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.11_1.ARMA.var <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.11_1.ARMA.var.C <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 1000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.5.11_2.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.11_2.ARMA <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.11_2.ARMA.var <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.11_2.ARMA.var.C <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 1000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)




I.8.5.11_3.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.11_3.ARMA <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 1000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.11_3.AR.var <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.11_3.AR.var.C <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                         random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                         control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                         correlation=corAR1(form=~year|country_code),na.action = na.omit)



AIC(I.8.5.11_1.AR, I.8.5.11_1.ARMA, I.8.5.11_1.ARMA.var, I.8.5.11_1.ARMA.var.C)

summary(I.8.5.11_1.ARMA.var.C)
plot(I.8.5.11_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)    # like previous one
qqnorm(I.8.5.11_1.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.11_1.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.5.11_1.ARMA.var.C, type = "normalized"))

AIC(I.8.5.11_2.AR, I.8.5.11_2.ARMA, I.8.5.11_2.ARMA.var, I.8.5.11_2.ARMA.var.C)

summary(I.8.5.11_2.ARMA.var.C)
plot(I.8.5.11_2.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)   # just as previous one
qqnorm(I.8.5.11_2.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.11_2.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.5.11_2.ARMA.var.C, type = "normalized"))

AIC(I.8.5.11_3.AR, I.8.5.11_3.ARMA, I.8.5.11_3.AR.var, I.8.5.11_3.AR.var.C)

summary(I.8.5.11_3.AR.var.C)
plot(I.8.5.11_3.AR.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)    # same but income_level acceptable
qqnorm(I.8.5.11_3.AR.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.11_3.AR.var.C, type = "normalized"))
pacf(resid(I.8.5.11_3.AR.var.C, type = "normalized"))


I.8.5.15_1.AR <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_1.ARMA <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_1.ARMA.var <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_1.ARMA.var.C <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



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

I.8.5.15_2.ARMA.var.C <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



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

I.8.5.15_3.ARMA.var.C <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                           random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                           control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                           correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



AIC(I.8.5.15_1.AR, I.8.5.15_1.ARMA, I.8.5.15_1.ARMA.var, I.8.5.15_1.ARMA.var.C)

summary(I.8.5.15_1.ARMA.var.C)
plot(I.8.5.15_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0) # income_level still shows heteroschedasticity
qqnorm(I.8.5.15_1.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.15_1.ARMA.var, type = "normalized"))
pacf(resid(I.8.5.15_1.ARMA.var, type = "normalized"))

AIC(I.8.5.15_2.AR, I.8.5.15_2.ARMA, I.8.5.15_2.ARMA.var, I.8.5.15_2.ARMA.var.C)

summary(I.8.5.15_2.ARMA.var.C)
plot(I.8.5.15_2.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0) # same as the previous models
qqnorm(I.8.5.15_2.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.15_2.ARMA.var.C, type = "normalized"))                          # still some autocorrelation
pacf(resid(I.8.5.15_2.ARMA.var.C, type = "normalized"))

AIC(I.8.5.15_3.AR, I.8.5.15_3.ARMA, I.8.5.15_3.ARMA.var, I.8.5.15_3.ARMA.var.C)

summary(I.8.5.15_3.ARMA.var.C)
plot(I.8.5.15_3.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0)   # country improves everything!
qqnorm(I.8.5.15_3.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.5.15_3.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.5.15_3.ARMA.var.C, type = "normalized"))


# I.8.5.15_2 <- lme(log(Indicator.8.5.15)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
#                 random=~1|country_code, data = mydata,
#                 correlation=corARMA(form=~year|country_code, p = 3, q = 0),na.action = na.omit)
# 
# AIC(I.8.5.15, I.8.5.15_2)
# summary(I.8.5.15_2)
# plot(I.8.5.15_2)
# qqnorm(I.8.5.15_2, ~ resid(., type = "p"), abline = c(0, 1))
# acf(resid(I.8.5.15_2, type = "normalized"))
# pacf(resid(I.8.5.15_2, type = "normalized"))
# 

I.8.10.10_1.AR <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.10.10_1.ARMA <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_1.ARMA.var <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                        correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_1.ARMA.var.C <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                            random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                            control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                            correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


I.8.10.10_2.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.10.10_2.ARMA <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_2.ARMA.var <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                        random=~1|country_code, data = mydata.log,  weights = varIdent(form = ~ 1 | income_level),
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                        correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_2.ARMA.var.C <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                            random=~1|country_code, data = mydata.log,  weights = varIdent(form = ~ 1 | country_code),
                            control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                            correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)



I.8.10.10_3.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.10.10_3.ARMA <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log,
                      control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                      correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_3.ARMA.var <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                        random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | income_level),
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                        correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.10.10_3.ARMA.var.C <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                            random=~1|country_code, data = mydata.log, weights = varIdent(form = ~ 1 | country_code),
                            control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                            correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)


AIC(I.8.10.10_1.AR, I.8.10.10_1.ARMA, I.8.10.10_1.ARMA.var, I.8.10.10_1.ARMA.var.C)


summary(I.8.10.10_1.ARMA.var)
plot(I.8.10.10_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0) # country improves a lot
plot(I.8.10.10_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.) | income_level, abline = 0) # country improves a lot
plot(I.8.10.10_1.ARMA.var.C, form = resid(., type = "n") ~ fitted(.) | country_code, abline = 0) # country improves a lot
qqnorm(I.8.10.10_1.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.10.10_1.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.10.10_1.ARMA.var.C, type = "normalized"))

AIC(I.8.10.10_2.AR, I.8.10.10_2.ARMA, I.8.10.10_2.ARMA.var, I.8.10.10_2.ARMA.var.C)

summary(I.8.10.10_2.ARMA.var.C)
plot(I.8.10.10_2.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0) # same as before
qqnorm(I.8.10.10_2.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.10.10_2.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.10.10_2.ARMA.var.C, type = "normalized"))

AIC(I.8.10.10_3.AR, I.8.10.10_3.ARMA, I.8.10.10_3.ARMA.var, I.8.10.10_3.ARMA.var.C)

summary(I.8.10.10_3.ARMA.var.C)
plot(I.8.10.10_3.ARMA.var.C, form = resid(., type = "n") ~ fitted(.), abline = 0) # ????????????????
qqnorm(I.8.10.10_3.ARMA.var.C, ~ resid(., type = "n"), abline = c(0, 1))
acf(resid(I.8.10.10_3.ARMA.var.C, type = "normalized"))
pacf(resid(I.8.10.10_3.ARMA.var.C, type = "normalized"))


# I.8.10.10_2 <- lme(log(Indicator.8.10.10)  ~ NFVD_prop + log(arrivals_int) + log(exp_int) + log(employ) + log(establishments),
#                  random=~1|country_code, data = mydata,
#                  correlation=corARMA(form=~year|country_code, p = 4),na.action = na.omit)
# 
# AIC(I.8.10.10, I.8.10.10_2)
# summary(I.8.10.10_2)
# plot(I.8.10.10_2)
# qqnorm(I.8.10.10_2, ~ resid(., type = "p"), abline = c(0, 1))
# acf(resid(I.8.10.10_2, type = "normalized"))
# pacf(resid(I.8.10.10_2, type = "normalized"))


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

