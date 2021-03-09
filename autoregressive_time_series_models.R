#********************************************************************************
#             
#				Time Series Analysis
#
#********************************************************************************

#************************************************************
# Reading in data
#************************************************************
library(ggplot2)
library(ggpubr)
library(ggbiplot)
library(ade4)
library(lattice) 
library(ggfortify)
library(ggResidpanel)
library(MASS)
library("GGally")
library(forecast)
library(lubridate)
require(dplyr) 

# Set working directory
datadir <- "~/Desktop/Fall 2020 Classes/SYS4021/Data/Spam"
sourcedir <-"~/Desktop/Fall 2020 Classes/SYS4021/Source"

#************************************************************
#
# Load the data & source files
#
#************************************************************
setwd(sourcedir)
source("AccidentInput.R")
source("SPM_Panel.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("pc.glm.R")
source("TestSet.R")

## Load the 'ham_ts.csv' and 'spam_ts.csv' into ham and spam variables repectively
setwd(datadir)
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')
setwd(sourcedir)

# Look at the data
dim(spam)
summary(spam)

dim(ham)
summary(ham)

#*****************************
# Seasonality
#*****************************
## Use the ts() command to get a time series of spam amount using spam data from all years
spam.ts<-ts(spam$count)

# Get the periodogram of spam
pg.spam <- spec.pgram(spam.ts,spans=9,demean=T,log='no')
pg.spam

# Find the peak, max.omega.precip
max.omega.spam<-pg.spam$freq[which(pg.spam$spec==max(pg.spam$spec))]

# Where is the peak?
max.omega.spam

# What is the period?
1/max.omega.spam

# What are the periods of the next biggest peaks?
# Sort spectrum from largest to smallest and find index
sorted.amp.spam <- sort(pg.spam$spec, decreasing=T, index.return=T)
sorted.f.spam <- pg.spam$freq[sorted.amp.spam$ix]
sorted.T.spam <- 1/sorted.f.spam
sorted.T.spam

## Use the ts() command to get a time series of ham amount using ham data from all years
ham.ts<-ts(ham$count)

# Get the periodogram of spam
pg.ham <- spec.pgram(ham.ts,spans=9,demean=T,log='no')

# Find the peak
max.omega.ham<-pg.ham$freq[which(pg.ham$spec==max(pg.ham$spec))]

# What is the period?
1/max.omega.ham

#************************************************************
#
#				Trend Analysis
#
#************************************************************
##Plot the time series you created for spam, spam.ts.
autoplot(spam.ts,ylab="Number of Spam Emails",xlab="Day")
length(spam.ts)

#### Model trend of spam
spam.short <- spam[1:357,]
spam.ts.short <-ts(spam$count[1:357])

# Create a new variable time.spam which is a matrix of (length(spam.ts))
time.spam<-c(1:(length(spam.ts.short)))

# Build a new model, spam.trend which predicts spam.ts based on the time variable, time.spam
spam.trend<-lm(spam.ts.short~time.spam)

#*****************************
# 
# AR, MA & ARIMA Models  
# 
#*****************************
# Get the residuals from the model above 
e.ts.spam <- ts(spam.trend$residuals)

# Plot the residuals
autoplot(e.ts.spam)

# Plot the autocorrelation (ACF) of the residuals of temp.trend.seasonal
spam.acf <- ggAcf(e.ts.spam)
spam.acf

# Plot the partial autocorrelation (PACF) of the residuals temp.trend.seasonal
spam.pacf <- ggPacf(e.ts.spam)
spam.pacf  

# Based on AIC, which one do you choose?
spam.auto <- auto.arima(e.ts.spam,approximation=FALSE)
summary(spam.auto)

# Choose p and q terms for e.ts.temp based on the acf and pacf 

# Fit an ARIMA(1,1,1) to 'e.ts.spam' time series
spam.arima111 <- arima(e.ts.spam, order=c(1,1,1), include.mean=FALSE)
summary(spam.arima111)

#Use auto.arima on all but the last 7 days of spam.ts
spam.auto1 <- auto.arima(spam.ts[1:(length(spam.ts)-7)], approximation=FALSE)
summary(spam.auto1)

# Plot the autocorrelation (ACF) and partial autocorrelation (PACF) of the residuals 
auto.resid.acf <- ggAcf(spam.auto1$residuals)
auto.resid.pacf <- ggPacf(spam.auto1$residuals)
ggarrange(auto.resid.acf,auto.resid.pacf,nrow=2,ncol=1)

# Plot diagnostics for independence of residuals 
ggtsdiag(spam.auto,gof.lag=20)
ggtsdiag(spam.auto1,gof.lag=20)
ggtsdiag(spam.arima111,gof.lag=20)

