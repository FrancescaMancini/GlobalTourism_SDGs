###################################################
# Script for graphical exploration and analysis
# of SDG indicators and tourism
# Author: Francesca Mancini
# Date created: 2017-12-06
# Date modified: 2018-02-08 
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

# Transformations ######

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
mydata.log$arrivals_int <- scale(log(mydata$arrivals_int))
mydata.log$exp_int <- scale(log(mydata$exp_int))
mydata.log$NFVD_prop <- scale(mydata$NFVD_prop)


# Tourism jobs

par(mfrow = c(1,2))
hist(mydata$employ, breaks = 100) 
hist(mydata$establishments, breaks = 100)   

dev.off()

mydata.log$employ <- scale(log(mydata$employ))
mydata.log$establishments <- scale(log(mydata$establishments))


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

mydata.log$Indicator.8.1.1 <- scale(mydata$Indicator.8.1.1)
mydata.log$Indicator.8.1.2 <- scale(mydata$Indicator.8.1.2)
mydata.log$Indicator.8.2.1 <- scale(log(mydata$Indicator.8.2.1))
mydata.log$Indicator.8.2.4 <- scale(log(mydata$Indicator.8.2.4))
mydata.log$Indicator.8.2.7 <- scale(mydata$Indicator.8.2.7)
mydata.log$Indicator.8.2.11 <-  scale(mydata$Indicator.8.2.11)
mydata.log$Indicator.8.5.5 <- scale(mydata$Indicator.8.5.5)
mydata.log$Indicator.8.5.11 <- scale(mydata$Indicator.8.5.11)
mydata.log$Indicator.8.5.15 <- scale(mydata$Indicator.8.5.15)
mydata.log$Indicator.8.10.10 <- scale(log(mydata$Indicator.8.10.10))

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

mydata.log$Indicator.12.2.1 <- scale(mydata$Indicator.12.2.1)

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


mydata.log$Indicator.14.4.1 <- scale(log(mydata$Indicator.14.4.1 + 1))
mydata.log$Indicator.14.4.2 <- scale(log(mydata$Indicator.14.4.2 + 1))
mydata.log$Indicator.14.4.3 <- scale(log(mydata$Indicator.14.4.3 + 1))


# Goal 15


par(mfrow = c(1,2))
hist(mydata$Indicator.15.1.1)
hist(mydata$Indicator.15.4.1)
hist(mydata.log$Indicator.15.5.1.x)

summary(mydata$Indicator.15.1.1)
summary(mydata$Indicator.15.4.1)
summary(mydata.log$Indicator.15.5.1.x)


mydata.log$Indicator.15.1.1 <- scale(mydata$Indicator.15.1.1)
mydata.log$Indicator.15.4.1 <- scale(mydata$Indicator.15.4.1)
mydata.log$Indicator.15.5.1 <- scale(asin(mydata$Indicator.15.5.1.x))  





# graphical exploration ######

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


# ===================================================
# Tourism volume and income
# ===================================================
# only using arrivals_int, trips_dom and exp_int 
# because other variables have too much missing data

###########################
# International arrivals
###########################


arrivals.AR <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

arrivals.ARMA <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(arrivals.AR, arrivals.ARMA)


grid.arrange(plot(arrivals.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(arrivals.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             qqnorm(arrivals.ARMA, ~ ranef(.)),
             plot(ACF(arrivals.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(arrivals.ARMA, type = "p"))
qqline(resid(arrivals.ARMA, type = "p"))

arrivals.ARMA.ML <- lme(arrivals_int ~ NFVD_prop, random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)



summary(arrivals.ARMA.ML)


##############################
# International expenditure
##############################

expend_int.AR <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

expend_int.ARMA <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log,
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(expend_int.AR, expend_int.ARMA)

grid.arrange(plot(expend_int.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(expend_int.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),             
             qqnorm(expend_int.ARMA, ~ ranef(.)),
             plot(ACF(expend_int.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(expend_int.ARMA, type = "p"))
qqline(resid(expend_int.ARMA, type = "p"))

expend_int.ARMA.ML <- lme(exp_int ~ NFVD_prop, random=~1|country_code, data = mydata.log, method = "ML",
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


summary(expend_int.ARMA.ML)


# =======================================================
# Tourism jobs
# =======================================================

tourism <- mydata.log[,c("trips_dom", "arrivals_int", "exp_int", "NFVD_prop")]

pairs(tourism, lower.panel = panel.cor, upper.panel = panel.smooth)

source("Collinearity.R")

# calculate VIF
VIF_tourism <- corvif(tourism)
VIF_tourism <- corvif(tourism[,-3])



##############################
# Employment
##############################


employment.1.AR <- lme(employ ~ NFVD_prop + arrivals_int ,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(employment.1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(employment.1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(employment.1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             qqnorm(employment.1.AR, ~ ranef(.)),
             plot(ACF(employment.1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(employment.1.AR, type = "p"))
qqline(resid(employment.1.AR, type = "p"))

employment.1.AR.ML <- lme(employ ~ NFVD_prop + arrivals_int ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(employment.1.AR.ML)


employment.2.AR <- lme(employ ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)



grid.arrange(plot(employment.2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(employment.2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(employment.2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(employment.2.AR, ~ ranef(.)),
             plot(ACF(employment.2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(employment.2.AR, type = "p"))
qqline(resid(employment.2.AR, type = "p"))

employment.2.AR.ML <- lme(employ ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(employment.2.AR.ML)

##############################
# Establishments
##############################


establishments.1.AR <- lme(establishments ~ NFVD_prop + arrivals_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit,
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

establishments.1.ARMA <- lme(establishments ~ NFVD_prop + arrivals_int, 
                               random=~1|country_code, data = mydata.log,
                               correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
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
             plot(ACF(establishments.1.ARMA, resType = "normalized"), alpha = .05))  

qqnorm(resid(establishments.1.ARMA, type = "p"))
qqline(resid(establishments.1.ARMA, type = "p"))

establishments.1.ARMA.ML <- lme(establishments ~ NFVD_prop + arrivals_int, 
                             random=~1|country_code, data = mydata.log, method = "ML",
                             correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                             control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

summary(establishments.1.ARMA.ML)



establishments.2.AR <- lme(establishments ~ NFVD_prop + exp_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corAR1(form=~year|country_code),na.action = na.omit)

establishments.2.ARMA <- lme(establishments ~ NFVD_prop + exp_int, 
                           random=~1|country_code, data = mydata.log,
                           correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                           control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

establishments.2.ARMA2 <- lme(establishments ~ NFVD_prop + exp_int, 
                             random=~1|country_code, data = mydata.log,
                             correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit,
                             control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

establishments.2.ARMA3 <- lme(establishments ~ NFVD_prop + exp_int, 
                              random=~1|country_code, data = mydata.log,
                              correlation=corARMA(form=~year|country_code, p = 1, q = 2),na.action = na.omit,
                              control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))



AIC(establishments.2.AR, establishments.2.ARMA, establishments.2.ARMA2, establishments.2.ARMA3)

grid.arrange(plot(establishments.2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(establishments.2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(establishments.2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(establishments.2.ARMA, ~ ranef(.)),
             plot(ACF(establishments.2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(establishments.2.ARMA, type = "p"))
qqline(resid(establishments.2.ARMA, type = "p"))

establishments.2.ARMA.ML <- lme(establishments ~ NFVD_prop + exp_int, 
                             random=~1|country_code, data = mydata.log, method = "ML",
                             correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                             control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000))

summary(establishments.2.ARMA.ML)


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



#######################
# GDP growth
#######################

I.8.1.1_1.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, 
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.1_1.ARMA <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log, 
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.1.1_1, I.8.1.1_1.AR, I.8.1.1_1.ARMA, I.8.1.1_1.ARMA2, I.8.1.1_1.ARMA3, I.8.1.1_1.ARMA4)

grid.arrange(plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.1.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.1.1_1.AR, ~ ranef(.)),
             plot(ACF(I.8.1.1_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.1_1.AR, type = "p"))
qqline(resid(I.8.1.1_1.AR, type = "p"))

I.8.1.1_1.AR.ML <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.1.1_1.AR.ML)




I.8.1.1_2.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.1_2.ARMA <- lme(Indicator.8.1.1 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.1.1_2.AR, I.8.1.1_2.ARMA, I.8.1.1_2.ARMA2, I.8.1.1_2.ARMA3)

grid.arrange(plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.1.1_2.AR, ~ ranef(.)),
             plot(ACF(I.8.1.1_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.1_2.AR, type = "p"))
qqline(resid(I.8.1.1_2.AR, type = "p"))

I.8.1.1_2.AR.ML <- lme(Indicator.8.1.1 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.1_2.AR.ML)




I.8.1.1_3.AR <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.1.1_3.ARMA <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log,
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

I.8.1.1_3.ARMA2 <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log,
                      correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)

I.8.1.1_3.ARMA3 <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,
                       correlation=corARMA(form=~year|country_code, p = 1, q = 2),na.action = na.omit)


AIC(I.8.1.1_3.AR, I.8.1.1_3.ARMA, I.8.1.1_3.ARMA2, I.8.1.1_3.ARMA3)

grid.arrange(plot(I.8.1.1_3.ARMA2, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.1_3.ARMA2, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.1_3.ARMA2, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.1.1_3.ARMA2, ~ ranef(.)),
             plot(ACF(I.8.1.1_3.ARMA2, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.1_3.ARMA2, type = "p"))
qqline(resid(I.8.1.1_3.ARMA2, type = "p"))

I.8.1.1_3.ARMA2.ML <- lme(Indicator.8.1.1 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)


summary(I.8.1.1_3.ARMA2.ML)


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
             plot(ACF(I.8.1.2_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.2_1.AR, type = "p"))
qqline(resid(I.8.1.2_1.AR, type = "p"))

I.8.1.2_1.AR.ML <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.1.2_1.AR.ML)


I.8.1.2_2.AR <- lme(Indicator.8.1.2 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.2_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.1.2_2.AR, ~ ranef(.)),
             plot(ACF(I.8.1.2_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.2_2.AR, type = "p"))
qqline(resid(I.8.1.2_2.AR, type = "p"))

I.8.1.2_2.AR.ML <- lme(Indicator.8.1.2 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.1.2_2.AR.ML)


I.8.1.2_3.AR <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.1.2_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.1.2_3.AR, ~ ranef(.)),
             plot(ACF(I.8.1.2_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.1.2_3.AR, type = "p"))
qqline(resid(I.8.1.2_3.AR, type = "p"))

I.8.1.2_3.AR.ML <- lme(Indicator.8.1.2 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.2_3.AR.ML)


###############################
# Employment in agriculture
###############################

I.8.2.1_1.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.1_1.ARMA <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,
                 correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                 control = list(maxIter = 100, msMaxIter = 100, niterEM = 100))



AIC(I.8.2.1_1.AR, I.8.2.1_1.ARMA)

grid.arrange(plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.1_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.1_1.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.1_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.1_1.ARMA, type = "p"))
qqline(resid(I.8.2.1_1.ARMA, type = "p"))

I.8.2.1_1.ARMA.ML <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log, method = "ML",
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                      control = list(maxIter = 100, msMaxIter = 100, niterEM = 100))

summary(I.8.2.1_1.ARMA.ML)


I.8.2.1_2.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.1_2.ARMA <- lme(Indicator.8.2.1 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.8.2.1_2.AR, I.8.2.1_2.ARMA, I.8.2.1_2.ARMA2, I.8.2.1_2.ARMA3)

grid.arrange(plot(I.8.2.1_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.1_2.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.1_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.1_2.ARMA, type = "p"))
qqline(resid(I.8.2.1_2.ARMA, type = "p"))


I.8.2.1_2.ARMA.ML <- lme(Indicator.8.2.1 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit, method = "ML")



summary(I.8.2.1_2.ARMA.ML)



I.8.2.1_3.AR <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.1_3.ARMA <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

I.8.2.1_3.ARMA2 <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)

I.8.2.1_3.ARMA3 <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 2),na.action = na.omit)

AIC(I.8.2.1_3.AR, I.8.2.1_3.ARMA, I.8.2.1_3.ARMA2, I.8.2.1_3.ARMA3)

grid.arrange(plot(I.8.2.1_3.ARMA2, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.1_3.ARMA2, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.1_3.ARMA2, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.1_3.ARMA2, ~ ranef(.)),
             plot(ACF(I.8.2.1_3.ARMA2, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.1_3.ARMA2, type = "p"))
qqline(resid(I.8.2.1_3.ARMA2, type = "p"))

I.8.2.1_3.ARMA2.ML <- lme(Indicator.8.2.1 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)


summary(I.8.2.1_3.ARMA2.ML)


###################################
# Employment in industry
###################################


I.8.2.4_1.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.4_1.ARMA <- lme(Indicator.8.2.4 ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code,  p = 1, q = 1),na.action = na.omit)

AIC(I.8.2.4_1.AR, I.8.2.4_1.ARMA)


grid.arrange(plot(I.8.2.4_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.4_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.4_1.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.4_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.4_1.ARMA, type = "p"))
qqline(resid(I.8.2.4_1.ARMA, type = "p"))

I.8.2.4_1.ARMA.ML <- lme(Indicator.8.2.4 ~ NFVD_prop + arrivals_int + establishments, method = "ML",
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code,  p = 1, q = 1),na.action = na.omit)

summary(I.8.2.4_1.ARMA.ML)


I.8.2.4_2.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.4_2.ARMA <- lme(Indicator.8.2.4 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.8.2.4_2.AR, I.8.2.4_2.ARMA)

grid.arrange(plot(I.8.2.4_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.4_2.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.4_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.4_2.ARMA, type = "p"))
qqline(resid(I.8.2.4_2.ARMA, type = "p"))

I.8.2.4_2.ARMA.ML <- lme(Indicator.8.2.4 ~ NFVD_prop + exp_int, method = "ML",
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.8.2.4_2.ARMA.ML)


I.8.2.4_3.AR <- lme(Indicator.8.2.4 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.4_3.ARMA <- lme(Indicator.8.2.4 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.8.2.4_3.AR, I.8.2.4_3.ARMA)


grid.arrange(plot(I.8.2.4_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.4_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.4_3.ARMA, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.2.4_3.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.4_3.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.4_3.ARMA, type = "p"))
qqline(resid(I.8.2.4_3.ARMA, type = "p"))

I.8.2.4_3.ARMA.ML <- lme(Indicator.8.2.4 ~ NFVD_prop + employ, method = "ML",
                      random=~1|country_code, data = mydata.log,control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


summary(I.8.2.4_3.ARMA.ML)

###############################
# Employment in services
###############################

I.8.2.7_1.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.7_1.ARMA <- lme(Indicator.8.2.7 ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.8.2.7_1.AR, I.8.2.7_1.ARMA)

grid.arrange(plot(I.8.2.7_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.7_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.7_1.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.7_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.7_1.ARMA, type = "p"))
qqline(resid(I.8.2.7_1.ARMA, type = "p"))

I.8.2.7_1.ARMA.ML <- lme(Indicator.8.2.7 ~ NFVD_prop + arrivals_int + establishments, method = "ML",
                      random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.8.2.7_1.ARMA.ML)

I.8.2.7_2.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.7_2.ARMA <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.2.7_2.AR, I.8.2.7_2.ARMA)

grid.arrange(plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.7_2.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.7_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.7_2.ARMA, type = "p"))
qqline(resid(I.8.2.7_2.ARMA, type = "p"))

I.8.2.7_2.ARMA.ML <- lme(Indicator.8.2.7 ~ NFVD_prop + exp_int, method = "ML",
                      random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.8.2.7_2.ARMA.ML)


I.8.2.7_3.AR <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.7_3.ARMA <- lme(Indicator.8.2.7 ~ NFVD_prop + employ,
                    random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.2.7_3.AR, I.8.2.7_3.ARMA)

grid.arrange(plot(I.8.2.7_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.7_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.7_3.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.7_3.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.7_3.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.7_3.ARMA, type = "p"))
qqline(resid(I.8.2.7_3.ARMA, type = "p"))

I.8.2.7_3.ARMA.ML <- lme(Indicator.8.2.7 ~ NFVD_prop + employ, method = "ML",
                      random=~1|country_code, data = mydata.log, control = list(maxIter = 100, msMaxIter = 100, niterEM = 100),
                      correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.8.2.7_3.ARMA.ML)

###############################
# GNI growth
###############################

I.8.2.11_1.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_1.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)



AIC(I.8.2.11_1.AR, I.8.2.11_1.ARMA)


grid.arrange(plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.2.11_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.2.11_1.AR, ~ ranef(.)),
             plot(ACF(I.8.2.11_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.11_1.AR, type = "p"))
qqline(resid(I.8.2.11_1.AR, type = "p"))

I.8.2.11_1.AR.ML <- lme(Indicator.8.2.11 ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.11_1.AR.ML)



I.8.2.11_2.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_2.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.2.11_2.AR, I.8.2.11_2.ARMA)

grid.arrange(plot(I.8.2.11_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.2.11_2.ARMA, ~ ranef(.)),
             plot(ACF(I.8.2.11_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.11_2.ARMA, type = "p"))
qqline(resid(I.8.2.11_2.ARMA, type = "p"))

I.8.2.11_2.ARMA.ML <- lme(Indicator.8.2.11 ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.8.2.11_2.ARMA.ML)


I.8.2.11_3.AR <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.2.11_3.ARMA <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,
                       control = list(maxIter = 1000, msMaxIter = 1000, niterEM = 1000, msMaxEval = 1000),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.8.2.11_3.AR, I.8.2.11_3.ARMA)

grid.arrange(plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.2.11_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.2.11_3.AR, ~ ranef(.)),
             plot(ACF(I.8.2.11_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.2.11_3.AR, type = "p"))
qqline(resid(I.8.2.11_3.AR, type = "p"))

I.8.2.11_3.AR.ML <- lme(Indicator.8.2.11 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.11_3.AR.ML)


################################
# Unemployment
################################

# Total unemployment



I.8.5.5_1.AR <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
               random=~1|country_code, data = mydata.log,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.5_1.ARMA <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log,
                    correlation=corARMA(form=~year|country_code, p = 1, q = 1), na.action = na.omit)



grid.arrange(plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.5_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.5_1.AR, ~ ranef(.)),
             plot(ACF(I.8.5.5_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.5_1.AR, type = "p"))
qqline(resid(I.8.5.5_1.AR, type = "p"))

I.8.5.5_1.AR.ML <- lme(Indicator.8.5.5  ~ NFVD_prop + arrivals_int + establishments,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.5_1.AR.ML)


I.8.5.5_2.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.5.5_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.5_2.AR, ~ranef(.)),
             plot(ACF(I.8.5.5_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.5_2.AR, type = "p"))
qqline(resid(I.8.5.5_2.AR, type = "p"))


I.8.5.5_2.AR.ML <- lme(Indicator.8.5.5 ~ NFVD_prop + exp_int,
                    random=~1|country_code, data = mydata.log, method = "ML",
                    correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.5_2.AR.ML)



I.8.5.5_3.AR <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.5_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.5_3.AR, ~ranef(.)),
             plot(ACF(I.8.5.5_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.5_3.AR, type = "p"))
qqline(resid(I.8.5.5_3.AR, type = "p"))

I.8.5.5_3.AR.ML <- lme(Indicator.8.5.5 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log, method = "ML",
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.5_3.AR.ML)

# Unemployment youth

I.8.5.11_1.AR <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.11_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.11_1.AR, ~ ranef(.)),
             plot(ACF(I.8.5.11_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.11_1.AR, type = "p"))
qqline(resid(I.8.5.11_1.AR, type = "p"))

I.8.5.11_1.AR.ML <- lme(Indicator.8.5.11  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.11_1.AR.ML)


I.8.5.11_2.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.11_2.AR, ~ ranef(.)),
             plot(ACF(I.8.5.11_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.11_2.AR, type = "p"))
qqline(resid(I.8.5.11_2.AR, type = "p"))

I.8.5.11_2.AR.ML <- lme(Indicator.8.5.11 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.11_2.AR.ML)



I.8.5.11_3.AR <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.11_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.11_3.AR, ~ ranef(.)),
             plot(ACF(I.8.5.11_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.11_3.AR, type = "p"))
qqline(resid(I.8.5.11_3.AR, type = "p"))

I.8.5.11_3.AR.ML <- lme(Indicator.8.5.11 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.11_3.AR.ML)


# Waged and salaried workers

I.8.5.15_1.AR <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.8.5.15_1.ARMA <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

I.8.5.15_1.ARMA2 <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 3, q = 1),na.action = na.omit)

I.8.5.15_1.ARMA3 <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log,
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                        correlation=corARMA(form=~year|country_code, p = 1, q = 3),na.action = na.omit)


AIC(I.8.5.15_1.AR, I.8.5.15_1.ARMA, I.8.5.15_1.ARMA2, I.8.5.15_1.ARMA3)

grid.arrange(plot(I.8.5.15_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.15_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.5.15_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.5.15_1.AR, ~ ranef(.)),
             plot(ACF(I.8.5.15_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.15_1.AR, type = "p"))
qqline(resid(I.8.5.15_1.AR, type = "p"))

I.8.5.15_1.AR.ML <- lme(Indicator.8.5.15  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.15_1.AR.ML)


I.8.5.15_2.AR <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

grid.arrange(plot(I.8.5.15_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.5.15_2.AR, ~ ranef(.)),
             plot(ACF(I.8.5.15_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.15_2.AR, type = "p"))
qqline(resid(I.8.5.15_2.AR, type = "p"))



I.8.5.15_2.AR.ML <- lme(Indicator.8.5.15 ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.15_2.AR.ML)



I.8.5.15_3.AR <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.5.15_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.5.15_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.5.15_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.5.15_3.AR, ~ ranef(.)),
             plot(ACF(I.8.5.15_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.5.15_3.AR, type = "p"))
qqline(resid(I.8.5.15_3.AR, type = "p"))

I.8.5.15_3.AR.ML <- lme(Indicator.8.5.15 ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.5.15_3.AR.ML)

#################################
# Access to banking
#################################

I.8.10.10_1.AR <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.10.10_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.8.10.10_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.8.10.10_1.AR, ~ ranef(.)),
             plot(ACF(I.8.10.10_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.10.10_1.AR, type = "p"))
qqline(resid(I.8.10.10_1.AR, type = "p"))

I.8.10.10_1.AR.ML <- lme(Indicator.8.10.10  ~ NFVD_prop + arrivals_int + establishments,
                      random=~1|country_code, data = mydata.log, method = "ML",
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.10.10_1.AR.ML)


I.8.10.10_2.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)



grid.arrange(plot(I.8.10.10_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.8.10.10_2.AR, ~ ranef(.)),
             plot(ACF(I.8.10.10_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.10.10_2.AR, type = "p"))
qqline(resid(I.8.10.10_2.AR, type = "p"))

I.8.10.10_2.AR.ML <- lme(Indicator.8.10.10 ~ NFVD_prop + exp_int,
                      random=~1|country_code, data = mydata.log, method = "ML",
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.10.10_2.AR.ML)


I.8.10.10_3.AR <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                   random=~1|country_code, data = mydata.log,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit)


grid.arrange(plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.8.10.10_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.8.10.10_3.AR, ~ ranef(.)),
             plot(ACF(I.8.10.10_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.8.10.10_3.AR, type = "p"))
qqline(resid(I.8.10.10_3.AR, type = "p"))

I.8.10.10_3.AR.ML <- lme(Indicator.8.10.10 ~ NFVD_prop + employ,
                      random=~1|country_code, data = mydata.log, method = "ML",
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.8.10.10_3.AR.ML)


#===========================================
# Goal 12
#===========================================


###########################################################
# Adjusted net savings, excl particulate emission damage
###########################################################

I.12.2.1_1.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + arrivals_int + establishments,
                 random=~1|country_code, data = mydata.log,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.12.2.1_1.ARMA <- lme(Indicator.12.2.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.12.2.1_1.AR, I.12.2.1_1.ARMA)

grid.arrange(plot(I.12.2.1_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.12.2.1_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.12.2.1_1.ARMA, ~ ranef(.)),
             plot(ACF(I.12.2.1_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.12.2.1_1.ARMA, type = "p"))
qqline(resid(I.12.2.1_1.ARMA, type = "p"))

I.12.2.1_1.ARMA.ML <- lme(Indicator.12.2.1  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


summary(I.12.2.1_1.ARMA.ML)


I.12.2.1_2.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.12.2.1_2.ARMA <- lme(Indicator.12.2.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.12.2.1_2.AR, I.12.2.1_2.ARMA)

grid.arrange(plot(I.12.2.1_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.12.2.1_2.ARMA, ~ ranef(.)),
             plot(ACF(I.12.2.1_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.12.2.1_2.ARMA, type = "p"))
qqline(resid(I.12.2.1_2.ARMA, type = "p"))


I.12.2.1_2.ARMA.ML <- lme(Indicator.12.2.1  ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


summary(I.12.2.1_2.ARMA.ML)


I.12.2.1_3.AR <- lme(Indicator.12.2.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.12.2.1_3.ARMA <- lme(Indicator.12.2.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log, 
                  correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))


AIC(I.12.2.1_3.AR, I.12.2.1_3.ARMA)

grid.arrange(plot(I.12.2.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.12.2.1_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.12.2.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.12.2.1_3.AR, ~ ranef(.)),
             plot(ACF(I.12.2.1_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.12.2.1_3.AR, type = "p"))
qqline(resid(I.12.2.1_3.AR, type = "p"))


I.12.2.1_3.AR.ML <- lme(Indicator.12.2.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)



summary(I.12.2.1_3.AR.ML)



#===========================================
# Goal 14
#===========================================


######################################
# Aquaculture production
######################################

I.14.4.1_1.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.1_1.ARMA <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, 
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1), na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))


AIC(I.14.4.1_1.AR, I.14.4.1_1.ARMA)

grid.arrange(plot(I.14.4.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0), 
             plot(I.14.4.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.1_1.AR, ~ ranef(.)),
             plot(ACF(I.14.4.1_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.1_1.AR, type = "p"))
qqline(resid(I.14.4.1_1.AR, type = "p"))


I.14.4.1_1.AR.ML <- lme(Indicator.14.4.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.14.4.1_1.AR.ML)




I.14.4.1_2.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.1_2.ARMA <- lme(Indicator.14.4.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.14.4.1_2.AR, I.14.4.1_2.ARMA)

grid.arrange(plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),  
             plot(I.14.4.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.1_2.AR, ~ ranef(.)),
             plot(ACF(I.14.4.1_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.1_2.AR, type = "p"))
qqline(resid(I.14.4.1_2.AR, type = "p"))


I.14.4.1_2.AR.ML <- lme(Indicator.14.4.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.14.4.1_2.AR.ML)




I.14.4.1_3.AR <- lme(Indicator.14.4.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.1_3.ARMA <- lme(Indicator.14.4.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.14.4.1_3.AR, I.14.4.1_3.ARMA)

grid.arrange(plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),            
             plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.1_3.AR, ~ ranef(.)),
             plot(ACF(I.14.4.1_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.1_3.AR, type = "p"))
qqline(resid(I.14.4.1_3.AR, type = "p"))


I.14.4.1_3.AR.ML <- lme(Indicator.14.4.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.14.4.1_3.AR.ML)

##########################
# Capture fisheries
##########################

I.14.4.2_1.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.2_1.ARMA <- lme(Indicator.14.4.2  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.14.4.2_1.AR, I.14.4.2_1.ARMA)

grid.arrange(plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.2_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.2_1.AR, ~ ranef(.)),
             plot(ACF(I.14.4.2_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.2_1.AR, type = "p"))
qqline(resid(I.14.4.2_1.AR, type = "p"))

I.14.4.2_1.AR.ML <- lme(Indicator.14.4.2  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.14.4.2_1.AR.ML)


I.14.4.2_2.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.2_2.ARMA <- lme(Indicator.14.4.2  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.14.4.2_2.AR, I.14.4.2_2.ARMA)

grid.arrange(plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.2_2.AR, ~ ranef(.)),
             plot(ACF(I.14.4.2_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.2_2.AR, type = "p"))
qqline(resid(I.14.4.2_2.AR, type = "p"))

I.14.4.2_2.AR.ML <- lme(Indicator.14.4.2  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.14.4.2_2.AR.ML)


I.14.4.2_3.AR <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.2_3.ARMA <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

I.14.4.2_3.ARMA2 <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit)

I.14.4.2_3.ARMA3 <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                        random=~1|country_code, data = mydata.log,
                        correlation=corARMA(form=~year|country_code, p = 1, q = 2),na.action = na.omit,
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))


AIC(I.14.4.2_3.AR, I.14.4.2_3.ARMA, I.14.4.2_3.ARMA2, I.14.4.2_3.ARMA3)

grid.arrange(plot(I.14.4.2_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.2_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.2_3.ARMA, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.2_3.ARMA, ~ ranef(.)),
             plot(ACF(I.14.4.2_3.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.2_3.ARMA, type = "p"))
qqline(resid(I.14.4.2_3.ARMA, type = "p"))

I.14.4.2_3.ARMA.ML <- lme(Indicator.14.4.2  ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.14.4.2_3.ARMA.ML)


#######################################
# Total fisheries production
#######################################

I.14.4.3_1.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.3_1.ARMA <- lme(Indicator.14.4.3  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.14.4.3_1.ARMA2 <- lme(Indicator.14.4.3  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log,
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.14.4.3_1.AR, I.14.4.3_1.ARMA, I.14.4.3_1.ARMA2)

grid.arrange(plot(I.14.4.3_1.ARMA2, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_1.ARMA2, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_1.ARMA2, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.14.4.3_1.ARMA2, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.14.4.3_1.ARMA2, ~ ranef(.)),
             plot(ACF(I.14.4.3_1.ARMA2, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.3_1.ARMA2, type = "p"))
qqline(resid(I.14.4.3_1.ARMA2, type = "p"))

I.14.4.3_1.ARMA2.ML <- lme(Indicator.14.4.3  ~ NFVD_prop + arrivals_int + establishments,
                        random=~1|country_code, data = mydata.log, method = "ML",
                        correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit,
                        control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.14.4.3_1.ARMA2.ML)



I.14.4.3_2.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.3_2.ARMA <- lme(Indicator.14.4.3  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.14.4.3_2.AR, I.14.4.3_2.ARMA)

grid.arrange(plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.14.4.3_2.AR, ~ ranef(.)),
             plot(ACF(I.14.4.3_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.3_2.AR, type = "p"))
qqline(resid(I.14.4.3_2.AR, type = "p"))

I.14.4.3_2.AR.ML <- lme(Indicator.14.4.3  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(I.14.4.3_2.AR.ML)


I.14.4.3_3.AR <- lme(Indicator.14.4.3  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.14.4.3_3.ARMA <- lme(Indicator.14.4.3  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.14.4.3_3.AR, I.14.4.3_3.ARMA)

grid.arrange(plot(I.14.4.3_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.14.4.3_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.14.4.3_3.ARMA, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.14.4.3_3.ARMA, ~ ranef(.)),
             plot(ACF(I.14.4.3_3.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.14.4.3_3.ARMA, type = "p"))
qqline(resid(I.14.4.3_3.ARMA, type = "p"))

I.14.4.3_3.ARMA.ML <- lme(Indicator.14.4.3  ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.14.4.3_3.ARMA.ML)


#===========================================
# Goal 15
#===========================================


#######################################
# Forest area
#######################################

I.15.1.1_1.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.1.1_1.ARMA <- lme(Indicator.15.1.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.15.1.1_1.AR, I.15.1.1_1.ARMA)

grid.arrange(plot(I.15.1.1_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.1.1_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.1.1_1.ARMA, ~ ranef(.)),
             plot(ACF(I.15.1.1_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.1.1_1.ARMA, type = "p"))
qqline(resid(I.15.1.1_1.ARMA, type = "p"))

I.15.1.1_1.ARMA.ML <- lme(Indicator.15.1.1  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.15.1.1_1.ARMA.ML)



I.15.1.1_2.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.15.1.1_2.ARMA <- lme(Indicator.15.1.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.15.1.1_2.AR, I.15.1.1_2.ARMA)

grid.arrange(plot(I.15.1.1_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.1.1_2.ARMA, ~ ranef(.)),
             plot(ACF(I.15.1.1_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.1.1_2.ARMA, type = "p"))
qqline(resid(I.15.1.1_2.ARMA, type = "p"))

I.15.1.1_2.ARMA.ML <- lme(Indicator.15.1.1  ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.15.1.1_2.ARMA.ML)


I.15.1.1_3.AR <- lme(Indicator.15.1.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.1.1_3.ARMA <- lme(Indicator.15.1.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

AIC(I.15.1.1_3.AR, I.15.1.1_3.ARMA)

grid.arrange(plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.1.1_3.ARMA, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.1.1_3.ARMA, ~ ranef(.)),
             plot(ACF(I.15.1.1_3.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.1.1_3.ARMA, type = "p"))
qqline(resid(I.15.1.1_3.ARMA, type = "p"))

I.15.1.1_3.ARMA.ML <- lme(Indicator.15.1.1  ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000),
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.15.1.1_3.ARMA.ML)


########################################
# Coverage by PA of important sites 
# for mountain biodiversity
########################################

I.15.4.1_1.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + arrivals_int + establishments,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.4.1_1.ARMA <- lme(Indicator.15.4.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.15.4.1_1.AR, I.15.4.1_1.ARMA)

grid.arrange(plot(I.15.4.1_1.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_1.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_1.ARMA, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.4.1_1.ARMA, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.4.1_1.ARMA, ~ ranef(.)),
             plot(ACF(I.15.4.1_1.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.4.1_1.ARMA, type = "p"))
qqline(resid(I.15.4.1_1.ARMA, type = "p"))

I.15.4.1_1.ARMA.ML <- lme(Indicator.15.4.1  ~ NFVD_prop + arrivals_int + establishments,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)

summary(I.15.4.1_1.ARMA.ML)


I.15.4.1_2.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + exp_int,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit,
                  control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.15.4.1_2.ARMA <- lme(Indicator.15.4.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.15.4.1_2.AR, I.15.4.1_2.ARMA)

grid.arrange(plot(I.15.4.1_2.ARMA, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_2.ARMA, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_2.ARMA, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.4.1_2.ARMA, ~ ranef(.)),
             plot(ACF(I.15.4.1_2.ARMA, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.4.1_2.ARMA, type = "p"))
qqline(resid(I.15.4.1_2.ARMA, type = "p"))

I.15.4.1_2.ARMA.ML <- lme(Indicator.15.4.1  ~ NFVD_prop + exp_int,
                       random=~1|country_code, data = mydata.log, method = "ML",
                       correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.15.4.1_2.ARMA.ML)




I.15.4.1_3.AR <- lme(Indicator.15.4.1  ~ NFVD_prop + employ,
                  random=~1|country_code, data = mydata.log,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.4.1_3.ARMA <- lme(Indicator.15.4.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit)


AIC(I.15.4.1_3.AR, I.15.4.1_3.ARMA)

grid.arrange(plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.4.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.4.1_3.AR, ~ ranef(.)),
             plot(ACF(I.15.4.1_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.4.1_3.AR, type = "p"))
qqline(resid(I.15.4.1_3.AR, type = "p"))

I.15.4.1_3.AR.ML <- lme(Indicator.15.4.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.15.4.1_3.AR.ML)


#######################################
# Red List Index
#######################################


I.15.5.1_1.AR <- lme(Indicator.15.5.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

I.15.5.1_1.ARMA <- lme(Indicator.15.5.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.15.5.1_1.AR, I.15.5.1_1.ARMA)

grid.arrange(plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ arrivals_int, abline = 0),
             plot(I.15.5.1_1.AR, form = resid(., type = "n") ~ establishments, abline = 0),
             qqnorm(I.15.5.1_1.AR, ~ ranef(.)),
             plot(ACF(I.15.5.1_1.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.5.1_1.AR, type = "p"))
qqline(resid(I.15.5.1_1.AR, type = "p"))

I.15.5.1_1.AR.ML <- lme(Indicator.15.5.1  ~ NFVD_prop + arrivals_int + establishments,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.15.5.1_1.AR.ML)


I.15.5.1_2.AR <- lme(Indicator.15.5.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.15.5.1_2.ARMA <- lme(Indicator.15.5.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))


AIC(I.15.5.1_2.AR, I.15.5.1_2.ARMA)

grid.arrange(plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_2.AR, form = resid(., type = "n") ~ exp_int, abline = 0),
             qqnorm(I.15.5.1_2.AR, ~ ranef(.)),
             plot(ACF(I.15.5.1_2.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.5.1_2.AR, type = "p"))
qqline(resid(I.15.5.1_2.AR, type = "p"))

I.15.5.1_2.AR.ML <- lme(Indicator.15.5.1  ~ NFVD_prop + exp_int,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.15.5.1_2.AR.ML)




I.15.5.1_3.AR <- lme(Indicator.15.5.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.15.5.1_3.ARMA <- lme(Indicator.15.5.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log,
                     correlation=corARMA(form=~year|country_code, p = 1, q = 1),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

I.15.5.1_3.ARMA2 <- lme(Indicator.15.5.1  ~ NFVD_prop + employ,
                       random=~1|country_code, data = mydata.log,
                       correlation=corARMA(form=~year|country_code, p = 2, q = 1),na.action = na.omit,
                       control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

AIC(I.15.5.1_3.AR, I.15.5.1_3.ARMA, I.15.5.1_3.ARMA2)

grid.arrange(plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ fitted(.), abline = 0),
             plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ NFVD_prop, abline = 0),
             plot(I.15.5.1_3.AR, form = resid(., type = "n") ~ employ, abline = 0),
             qqnorm(I.15.5.1_3.AR, ~ ranef(.)),
             plot(ACF(I.15.5.1_3.AR, resType = "normalized"), alpha = .05))

qqnorm(resid(I.15.5.1_3.AR, type = "p"))
qqline(resid(I.15.5.1_3.AR, type = "p"))

I.15.5.1_3.AR.ML <- lme(Indicator.15.5.1  ~ NFVD_prop + employ,
                     random=~1|country_code, data = mydata.log, method = "ML",
                     correlation=corAR1(form=~year|country_code),na.action = na.omit,
                     control = list(maxIter = 10000, msMaxIter = 10000, niterEM = 10000, msMaxEval = 10000))

summary(I.15.5.1_3.AR.ML)


