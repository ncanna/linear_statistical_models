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

#************************************************************
#
#				Graphical Analysis
#
#************************************************************

## Use the ts() command to get a time series of ham amount using ham data from all years
ham.ts<-ts(ham$count)

## Use the ts() command to get a time series of spam amount using spam data from all years
spam.ts<-ts(spam$count)

## Plot the time series created for ham
autoplot(ham.ts,ylab="Number of Ham Emails",xlab="Day")

date <- ham %>% select(year, month, day) %>% mutate(date = make_datetime(year,month,day))
ham$date <- date$date
ggplot(ham, aes(x=date,y=count)) + geom_line() + ylab("Ham Count") + xlab("")

## Plot the time series created for spam
autoplot(spam.ts,ylab="Number of Spam Emails",xlab="Day")

date <- spam %>% select(year, month, day) %>% mutate(date = make_datetime(year,month,day))
spam$date <- date$date
ggplot(spam, aes(x=date,y=count)) + geom_line() + ylab("Spam Count") + xlab("")

#### Model trend of spam
# Create a new variable time.spam which is a matrix of (length(spam.ts))
time.spam<-c(1:(length(spam.ts)))

# Build a new model, spam.trend which predicts spam.ts based on the time variable, time.spam
spam.trend<-lm(spam.ts~time.spam)

## Use the summary() command on spam.trend. Is time significant in predicting spam frequency?
summary(spam.trend)

# Plot the trend line for spam.ts
plot(spam.ts)
abline(spam.trend,col='red')

ggplot(spam, aes(x=date,y=count)) + geom_line() + stat_smooth(method="lm",col="red") + ylab("Spam Emails")

# Use the acf() command on created time series for spam
acf(spam.ts)
ggAcf(spam.ts)

#************************************************************
# Observations
#     &  
# Model trend, seasonality
#************************************************************

autoplot(ham.ts,ylab="Number of Ham Emails",xlab="Day")
length(ham.ts)

# Modify  ham time series object by removing the last six weeks 
post_ham.ts <- ts(ham$count[1:464])
autoplot(post_ham.ts)
post_ham <- ham[1:464,]

#### Model trend of post ham
# Build a new model, spam.trend which predicts spam.ts based on the time variable, time.spam
time.post_ham<-c(1:(length(post_ham.ts)))
post_ham.trend<-lm(post_ham.ts~time.post_ham)
summary(post_ham.trend)

ggplot(ham, aes(x=date,y=count)) + geom_line() + stat_smooth(method="lm",col="red") + ylab("Ham Emails")

# Use the acf() command on created time series for post ham
acf(post_ham.ts)
ggAcf(post_ham.ts)

# Model the seasonality for ham data set using dummy variables. Use day of the week as the interval.
post_ham.day <- time.post_ham %% 7
post_ham.day <-as.factor(time.post_ham %% 7) 

# January 13, 2000, Thursday is first day of ham e-mail time series. Change levels into text for understanding
Day <- rep(NA, length(post_ham.ts))
Day[which((time.post_ham %% 7)    == 1)] <- "Th"  
Day[which((time.post_ham %% 7)    == 2)] <- "F"
Day[which((time.post_ham %% 7)    == 3)] <- "Sa"
Day[which((time.post_ham %% 7)    == 4)] <- "S"
Day[which((time.post_ham %% 7)    == 5)] <- "M"
Day[which((time.post_ham %% 7)    == 6)] <- "T"
Day[which((time.post_ham %% 7)    == 0)] <- "W"
Day <- as.factor(Day)

# Check base case
contrasts(Day)

# Model the trend and seasonality of ham
ham.trendseason<-lm(post_ham.ts~time.post_ham+Day)
summary(ham.trendseason) 

ham.season<-lm(post_ham.ts~Day)
summary(ham.season) 

# Compare models
anova(ham.trendseason, ham.season, test = "Chi")