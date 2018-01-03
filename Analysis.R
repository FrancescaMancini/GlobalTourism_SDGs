###################################################
# Script for graphical exploration and analysis
# of SDG indicators and tourism
# Author: Francesca Mancini
# Date created: 2017-12-06
# Date modified: 
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
# Tourism income
# ===================================================
intguests <- lme(log(guests_int) ~ NFVD_prop, random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)


summary(intguests)
plot(intguests)

domguests <- lme(log(guests_dom) ~ NFVD_prop, random=~1|country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(domguests)
plot(domguests)

domtrips <- lme(log(trips_dom) ~ NFVD_prop, random=~1|country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(domtrips)
plot(domtrips)

arrivals <- lme(log(arrivals_int) ~ NFVD_prop, random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(arrivals)
plot(arrivals)

expend_int <- lme(log(exp_int) ~ NFVD_prop, random=~1|country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(expend_int)
plot(expend_int)

expend_dom <- lme(log(exp_dom) ~ NFVD_prop, random=~1|country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(expend_dom)
plot(expend_dom)

# =======================================================
# Tourism jobs
# =======================================================

employment <- lme(log(employ) ~ NFVD_prop, random=~1|country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(employment)
plot(employment)

agencies <- lme(log(agencies) ~ NFVD_prop, random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(agencies)
plot(agencies)

establishments <- lme(log(establishments) ~ NFVD_prop, random=~1|country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(establishments)
plot(establishments)

#===========================================
# Goal 8
#===========================================
I.8.1.1 <- lme(Indicator.8.1.1 ~ NFVD_prop + arrivals_int + exp_int + guests_dom,
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.1)
plot(I.8.1.1)

I.8.1.2 <- lme(Indicator.8.1.2 ~ NFVD_prop + arrivals_int + exp_int + guests_dom,
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.1.2)
plot(I.8.1.2)

I.8.2.1 <- lme(Indicator.8.2.1 ~ NFVD_prop + arrivals_int + exp_int + guests_dom,
               random=~1|country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit)

summary(I.8.2.1)
plot(I.8.2.1)



# ===============================================
# Piecewise SEM
# ===============================================
library(piecewiseSEM)
library(nlme)

# transform variables

vars <- c("guests_dom", "trips_dom", "exp_dom", "employ", "guests_int",
          "arrivals_int", "exp_int", "establishments", "agencies", "Indicator.8.2.1",
          "Indicator.8.2.4", "Indicator.8.2.7", "Indicator.8.4.2", "Indicator.8.5.5", 
          "Indicator.8.5.11", "Indicator.8.5.15", "Indicator.8.8.1", "Indicator.8.8.4",
          "Indicator.8.10.10")

mydata[vars] <- log(mydata[vars]+1)


# predicting jobs with nature-based tourism

T.jobs = list(
  
  # Predicting employment
  employ = lme(employ ~ NFVD_prop + establishments, 
                random = ~ 1 | country_code, data = mydata,
                correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Predicting number of establishments
  establishments = lme(establishments ~ NFVD_prop, 
                       random = ~ 1 | country_code, data = mydata,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Predicting number of agencies
  agencies = lme(agencies ~ NFVD_prop + establishments, 
                 random = ~ 1 | country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit)
)

# Run goodness-of-fit tests
sem.fit(T.jobs, get.scaled.data(T.jobs, mydata, standardize = "scale"))


# Evaluate path significance using unstandardized coefficients
sem.coefs(T.jobs, mydata, standardize = "none")


# Obtain standardized regression coefficients
sem.coefs(T.jobs, mydata, standardize = "scale")

# Explore individual model fits
sem.model.fits(T.jobs)


sem.fit(T.jobs, mydata)$missing.paths


# predicting tourism volume with nature-based tourism
T.volume = list(
  
  # Predicting employment
  guests_dom = lme(guests_dom ~ NFVD_prop + trips_dom, 
               random = ~ 1 | country_code, data = mydata,
               correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Predicting number of establishments
  trips_dom = lme(trips_dom ~ NFVD_prop + guests_dom, 
                       random = ~ 1 | country_code, data = mydata,
                       correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Predicting number of agencies
  arrivals_int = lme(arrivals_int ~ NFVD_prop + guests_int, 
                 random = ~ 1 | country_code, data = mydata,
                 correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  guests_int = lme(guests_int ~ NFVD_prop + arrivals_int, 
                     random = ~ 1 | country_code, data = mydata,
                     correlation=corAR1(form=~year|country_code),na.action = na.omit)
)

# Run goodness-of-fit tests
sem.fit(T.volume, get.scaled.data(T.volume, mydata, standardize = "scale"))


# Evaluate path significance using unstandardized coefficients
sem.coefs(T.volume, mydata, standardize = "none")


# Obtain standardized regression coefficients
sem.coefs(T.volume, get.scaled.data(T.volume, mydata, standardize = "scale"), standardize = "scale")

# Explore individual model fits
sem.model.fits(T.volume)


# predicting tourism income
T.expenditure = list(
  
  # Predicting employment
  exp_dom = lme(exp_dom ~ NFVD_prop + trips_dom + guests_dom, 
                   random = ~ 1 | country_code, data = mydata,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Predicting number of establishments
  exp_int = lme(exp_int ~ NFVD_prop + guests_int + arrivals_int, 
                  random = ~ 1 | country_code, data = mydata,
                  correlation=corAR1(form=~year|country_code),na.action = na.omit)
)

# Run goodness-of-fit tests
sem.fit(T.expenditure, get.scaled.data(T.expenditure, mydata, standardize = "scale"))


# Evaluate path significance using unstandardized coefficients
sem.coefs(T.expenditure, get.scaled.data(T.expenditure, mydata, standardize = "scale"), standardize = "none")


# Obtain standardized regression coefficients
sem.coefs(T.expenditure, mydata, standardize = "scale")

# Explore individual model fits
sem.model.fits(T.expenditure)

ctrl <- lmeControl(maxIter = 1000, msMaxIter = 1000, tolerance = 1e-5)

# predicting economic growth
Goal8 = list(
  
  # Predicting target 1
  Indicator.8.1.1 = lme(Indicator.8.1.1 ~ NFVD_prop + guests_int + guests_dom +
                        exp_int + exp_int + employ, 
                   random = ~ 1 | country_code, data = mydata, control = ctrl,
                   correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.8.1.2 = lme(Indicator.8.1.2 ~ NFVD_prop + guests_int +guests_dom +
                          exp_int + exp_int + employ, 
                        random = ~ 1 | country_code, data = mydata, control = ctrl,
                        correlation=corAR1(form=~year|country_code),na.action = na.omit),
  # Target 2
  Indicator.8.2.1 = lme(Indicator.8.2.1 ~ NFVD_prop + employ, 
                        random = ~ 1 | country_code, data = mydata, control = ctrl,
                        correlation=corAR1(form=~year|country_code),na.action = na.omit),

  Indicator.8.2.4 = lme(Indicator.8.2.4 ~ NFVD_prop + employ + establishments + agencies, 
                        random = ~ 1 | country_code, data = mydata, control = ctrl,
                        correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.8.2.7 = lme(Indicator.8.2.7 ~ NFVD_prop + employ + agencies + establishments, 
                        random = ~ 1 | country_code, data = mydata, control = ctrl,
                        correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Indicator.8.2.10 = lme(Indicator.8.2.10 ~ NFVD_prop + guests_int + guests_dom +
  #                       exp_int + exp_int + employ, 
  #                       random = ~ 1 | country_code, data = mydata, control = ctrl,
  #                       correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.8.2.11 = lme(Indicator.8.2.11 ~ NFVD_prop + guests_int + guests_dom +
                         exp_int + exp_int + employ,  control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
# Target 3
# Indicator.8.3.1 = lme(Indicator.8.3.1 ~ NFVD_prop + employ + 
#                       establishments + agencies, 
#                       random = ~ 1 | country_code, data = mydata,
#                       correlation=corAR1(form=~year|country_code),na.action = na.omit),

Indicator.8.3.4 = lme(Indicator.8.3.4 ~ NFVD_prop + employ + 
                      establishments + agencies,  control = ctrl,
                      random = ~ 1 | country_code, data = mydata,
                      correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Target 4
# Indicator.8.4.1 = lme(log(Indicator.8.4.1) ~ NFVD_prop + log(trips_dom) + log(guests_dom), 
#                       random = ~ 1 | country_code, data = mydata,
#                       correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Indicator.8.4.2 = lme(Indicator.8.4.2 ~ NFVD_prop + trips_dom + guests_dom, 
#                        random = ~ 1 | country_code, data = mydata,  control = ctrl,
#                        correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Target 5
Indicator.8.5.5 = lme(Indicator.8.5.5 ~ NFVD_prop + employ +
                      establishments + agencies, control = ctrl,
                      random = ~ 1 | country_code, data = mydata,
                      correlation=corAR1(form=~year|country_code),na.action = na.omit),

Indicator.8.5.11 = lme(Indicator.8.5.11 ~ NFVD_prop + employ +
                        establishments + agencies, control = ctrl, 
                      random = ~ 1 | country_code, data = mydata,
                      correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Indicator.8.5.15 = lme(Indicator.8.5.15 ~ NFVD_prop + employ +
#                          establishments + agencies, control = ctrl, 
#                        random = ~ 1 | country_code, data = mydata,
#                        correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Target 6
Indicator.8.6.3 = lme(Indicator.8.6.3 ~ NFVD_prop + employ + 
                      establishments + agencies, control = ctrl,
                      random = ~ 1 | country_code, data = mydata,
                      correlation=corAR1(form=~year|country_code),na.action = na.omit)

# Target 7
# Indicator.8.7.3 = lme(Indicator.8.7.3 ~ NFVD_prop + employ + 
#                       establishments + agencies,
#                       random = ~ 1 | country_code, data = mydata,
#                       correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Target 8
# Indicator.8.8.1 = lme(Indicator.8.8.1 ~ NFVD_prop + employ + 
#                       establishments + agencies, control = ctrl,
#                       random = ~ 1 | country_code, data = mydata,
#                       correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Indicator.8.8.4 = lme(Indicator.8.8.4 ~ NFVD_prop + employ + 
#                         establishments + agencies, control = ctrl,
#                       random = ~ 1 | country_code, data = mydata,
#                       correlation=corAR1(form=~year|country_code),na.action = na.omit),

# Target 10
# Indicator.8.10.10 = lme(Indicator.8.10.10 ~ NFVD_prop + employ +
#                         exp_dom + exp_int, control = ctrl,
#                         random = ~ 1 | country_code, data = mydata,
#                         correlation=corAR1(form=~year|country_code),na.action = na.omit)
)

# Run goodness-of-fit tests
sem.fit(Goal8, get.scaled.data(Goal8, mydata, standardize = "scale")
        , corr.errors = c("Indicator.8.1.1 ~~ Indicator 8.1.2",
                          "Indicator.8.2.1 ~~ Indicator.8.2.4",
                          "Indicator.8.2.1 ~~ Indicator.8.2.7",
                          "Indicator.8.2.1 ~~ Indicator.8.2.11",
                          "Indicator.8.2.4 ~~ Indicator.8.2.7",
                          "Indicator.8.2.4 ~~ Indicator.8.2.11",
                          "Indicator.8.2.7 ~~ Indicator.8.2.11",
                          "Indicator.8.5.5 ~~ Indicator.8.5.11",
                          "Indicator.8.2.1 ~~ Indicator.8.5.5",
                          "Indicator.8.2.1 ~~ Indicator.8.5.11",
                          "Indicator.8.2.4 ~~ Indicator.8.5.5",
                          "Indicator.8.2.4 ~~ Indicator.8.5.11",
                          "indicator.8.2.7 ~~ Indicator.8.5.5",
                          "Indicator.8.2.7 ~~ Indicator.8.5.11",
                          "Indicator.8.2.11 ~~ Indicator.8.5.5",
                          "Indicator.8.2.11 ~~ Indicator.8.5.11",
                          "guests_int ~~ arrivals_int",
                          "guests_int ~~ exp_int",
                          "arrivals_int ~~ exp_int",
                          "guests_dom ~~ trips_dom",
                          "guests_dom ~~ exp_dom",
                          "trips_dom ~~ exp_dom",
                          "employ ~~ establishments",
                          "employ ~~ agencies"))



# Evaluate path significance using unstandardized coefficients
sem.coefs(Goal8, get.scaled.data(Goal8, mydata, standardize = "scale"), standardize = "none")


# Obtain standardized regression coefficients
#sem.coefs(Goal8, mydata, standardize = "scale")

# Explore individual model fits
sem.model.fits(Goal8)

# predicting sustainable consumption
vars <- c("Indicator.12.2.2", "Indicator.12.2.3", "Indicator.12.2.4",
          "Indicator.12.2.5", "Indicator.12.2.6", "Indicator.12.2.7")

mydata[vars] <- log(mydata[vars]+1)



Goal12 <- list(
  
  Indicator.12.2.1 = lme(Indicator.12.2.1 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.12.2.2 = lme(Indicator.12.2.2 ~ NFVD_prop + arrivals_int + guests_int +
                          trips_dom + guests_dom + exp_dom + exp_int + employ +
                          establishments + agencies, control = ctrl,
                        random = ~ 1 | country_code, data = mydata,
                        correlation=corAR1(form=~year|country_code),na.action = na.omit),

  Indicator.12.2.3 = lme(Indicator.12.2.3 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.12.2.4 = lme(Indicator.12.2.4 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.12.2.5 = lme(Indicator.12.2.5 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.12.2.6 = lme(Indicator.12.2.6 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.12.2.7 = lme(Indicator.12.2.7 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit)
)


# Run goodness-of-fit tests
sem.fit(Goal12, get.scaled.data(Goal12, mydata, standardize = "scale"),
        corr.errors = c("guests_int ~~ arrivals_int",
                          "guests_int ~~ exp_int",
                          "arrivals_int ~~ exp_int",
                          "guests_dom ~~ trips_dom",
                          "guests_dom ~~ exp_dom",
                          "trips_dom ~~ exp_dom",
                          "employ ~~ establishments",
                          "employ ~~ agencies",
                          "Indicator.12.2.7 ~~ Indicator.12.2.1",
                          "Indicator.12.2.7 ~~ Indicator.12.2.2",
                          "Indicator.12.2.7 ~~ Indicator.12.2.3",
                          "Indicator.12.2.7 ~~ Indicator.12.2.4",
                          "Indicator.12.2.7 ~~ Indicator.12.2.5",
                          "Indicator.12.2.7 ~~ Indicator.12.2.6"))



# Evaluate path significance using unstandardized coefficients
sem.coefs(Goal12, get.scaled.data(Goal12, mydata, standardize = "scale"), standardize = "none")


# Obtain standardized regression coefficients
#sem.coefs(Goal8, mydata, standardize = "scale")

# Explore individual model fits
sem.model.fits(Goal12)


# predicting sustainable oceans
vars <- c("Indicator.14.4.1", "Indicator.14.4.2", "Indicator.14.4.3")

mydata[vars] <- log(mydata[vars]+1)



Goal14 <- list(
  
  Indicator.14.4.1 = lme(Indicator.14.4.1 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  # Indicator.14.4.2 = lme(Indicator.14.4.2 ~ NFVD_prop + arrivals_int + guests_int +
  #                          trips_dom + guests_dom + exp_dom + exp_int + employ +
  #                          establishments + agencies, control = ctrl,
  #                        random = ~ 1 | country_code, data = mydata,
  #                        correlation=corAR1(form=~year|country_code),na.action = na.omit),
  
  Indicator.14.4.3 = lme(Indicator.14.4.3 ~ NFVD_prop + arrivals_int + guests_int +
                           trips_dom + guests_dom + exp_dom + exp_int + employ +
                           establishments + agencies, control = ctrl,
                         random = ~ 1 | country_code, data = mydata,
                         correlation=corAR1(form=~year|country_code),na.action = na.omit)
  )

sem.fit(Goal14, get.scaled.data(Goal14, mydata, standardize = "scale"),
        corr.errors = c("guests_int ~~ arrivals_int",
                        "guests_int ~~ exp_int",
                        "arrivals_int ~~ exp_int",
                        "guests_dom ~~ trips_dom",
                        "guests_dom ~~ exp_dom",
                        "trips_dom ~~ exp_dom",
                        "employ ~~ establishments",
                        "employ ~~ agencies",
                        "Indicator.14.4.2 ~~ Indicator.14.4.3"))



# Evaluate path significance using unstandardized coefficients
sem.coefs(Goal14, get.scaled.data(Goal14, mydata, standardize = "scale"), standardize = "none")


# Obtain standardized regression coefficients
#sem.coefs(Goal8, mydata, standardize = "scale")

# Explore individual model fits
sem.model.fits(Goal14)



# ================================================
# Confirmatory Factor Analysis - Tourism
# ================================================
library(lavaan)
library(semPlot)
library(semTools)

varslog <- c("guests_dom", "trips_dom", "exp_dom", "employ", "guests_int", "Indicator.8.4.1",
          "arrivals_int", "exp_int", "establishments", "agencies", "Indicator.8.2.1",
          "Indicator.8.2.4", "Indicator.8.2.10", "Indicator.8.3.4", "Indicator.8.4.2", 
          "Indicator.8.5.5", "Indicator.8.5.11", "Indicator.8.8.1", "Indicator.8.7.3",
          "Indicator.8.10.10", "Indicator.12.2.2", "Indicator.12.2.3", "Indicator.12.2.4",
          "Indicator.12.2.5", "Indicator.12.2.6", "Indicator.12.2.7", "Indicator.14.4.1",
          "Indicator.14.4.2", "Indicator.14.4.3", "Indicator.15.2.3", "Indicator.15.2.6")

varssqrt <- "Indicator.8.8.4"

mydata[varslog] <- log(mydata[varslog]+1)

mydata[varssqrt] <- sqrt(mydata[varssqrt])

# something might not be working here because things are not normal after log

mydata[,3:49] <- scale(mydata[, 3:49], center = T, scale = T)

omar <- par()$mar
omar
par(mfrow = c(3, 4), mar = c(4, 3, 2, 2))
for (i in names(mydata[,3:12])) hist(mydata[ , i], main = i)
par(mfrow = c(1, 1), mar = omar)


T.model <- "T.income =~ exp_dom + exp_int + guests_dom + trips_dom + arrivals_int + guests_int 
           Eco =~ NFVD_prop
           T.jobs =~ employ + establishments + agencies" 

T.model.fit <- cfa(T.model, data=mydata, std.lv=TRUE, missing="fiml") 

summary(T.model.fit, fit.measures=TRUE, standardized=TRUE)

parameterEstimates(T.model.fit, standardized=TRUE)

residuals(T.model.fit, type = "cor")$cor

modificationIndices(T.model.fit, sort.=TRUE, minimum.value=3)


T.model.fit.nc <- cfa(T.model, data=mydata, std.lv=TRUE, missing="fiml", orthogonal = T) 

anova(T.model.fit, T.model.fit.nc)

T.model.one <- "Tourism =~ exp_dom + exp_int + guests_dom + trips_dom + arrivals_int + 
                guests_int + NFVD_prop + employ + establishments + agencies"

T.model.fit.one <- cfa(T.model.one, data=mydata, std.lv=TRUE, missing="fiml")

anova(T.model.fit, T.model.fit.one)


T.model.four <- "T.volume =~ guests_dom + trips_dom + arrivals_int +guests_int 
                T.income =~ exp_dom + exp_int 
                T.jobs =~ employ + establishments + agencies
                Eco =~ NFVD_prop"

T.model.fit.four <- cfa(T.model.four, data=mydata, std.lv=TRUE, missing="fiml", orthogonal = T)

anova(T.model.fit, T.model.fit.four)

T.model.intVSdom <- "T.int =~ arrivals_int + guests_int + exp_int
                     T.dom =~ trips_dom + guests_dom + exp_dom
                     T.jobs =~ employ + establishments + agencies
                     Eco =~ NFVD_prop"

T.model.fit.intVSdom <- cfa(T.model.intVSdom, data=mydata, std.lv=TRUE, missing="fiml", orthogonal = T)

anova(T.model.fit, T.model.fit.intVSdom)

T.model.two <- "Tourism =~ guests_dom + trips_dom + arrivals_int +guests_int +
                exp_dom + exp_int + employ + establishments + agencies
                Eco =~ NFVD_prop"

T.model.fit.two <- cfa(T.model.two, data=mydata, std.lv=TRUE, missing="fiml")

anova(T.model.fit, T.model.fit.two)

summary(T.model.fit.two, fit.measures=TRUE, standardized=TRUE)

semPaths(T.model.fit, "std")


T.model.volume <- "T.int =~ arrivals_int + guests_int
                   T.dom =~ trips_dom + guests_dom 
                   Eco =~ NFVD_prop"

T.model.fit.volume <- cfa(T.model.volume, data=mydata, std.lv=TRUE, missing="fiml")

summary(T.model.fit.volume, fit.measures=TRUE, standardized=TRUE)

semPaths(T.model.fit.volume, "std")

parameterEstimates(T.model.fit.volume, standardized=TRUE)

residuals(T.model.fit.volume, type = "cor")$cor

modificationIndices(T.model.fit.volume, sort.=TRUE)

# ================================================
# Confirmatory Factor Analysis - Goal 8
# ================================================
omar <- par()$mar
omar
par(mfrow = c(4, 5), mar = c(4, 3, 2, 2))
for (i in names(mydata[,31:49])) hist(mydata[ , i], main = i)
par(mfrow = c(1, 1), mar = omar)


Goal8.1 <- "Goal8 =~ E.growth + Jobs + Decent.work + Decoupling

            E.growth =~ Indicator.8.1.1 + Indicator.8.1.2 + Indicator.8.2.11

            Jobs =~ Indicator.8.2.1 + Indicator.8.2.4 + Indicator.8.2.7 +
            Indicator.8.3.4 

            #Unemployment =~ Indicator.8.5.5 + Indicator.8.5.11 + Indicator.8.6.3

            Decent.work =~ Indicator.8.3.1 + Indicator.8.8.1 + 
            Indicator.8.8.4 + Indicator.8.7.3 + Indicator.8.5.15

            Decoupling =~ Indicator.8.4.1 + Indicator.8.4.2"

Goal8.1.fit <- cfa(Goal8.1, data=mydata, missing="fiml")

summary(Goal8.1.fit, fit.measures=TRUE, standardized=TRUE)

semPaths(Goal8.1.fit, "std")

parameterEstimates(Goal8.1.fit, standardized=TRUE)

residuals(Goal8.1.fit, type = "cor")$cor

modificationIndices(Goal8.1.fit, sort.=TRUE)

# acceptable fit but could be improved


# ================================================
# Confirmatory Factor Analysis - Goal 12
# ================================================
omar <- par()$mar
omar
par(mfrow = c(4, 2), mar = c(4, 3, 2, 2))
for (i in names(mydata[,13:19])) hist(mydata[ , i], main = i)
par(mfrow = c(1, 1), mar = omar)


