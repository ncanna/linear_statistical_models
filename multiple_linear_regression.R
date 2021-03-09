#********************************************************************************
#             
#				Multiple Linear Regression
#
#********************************************************************************

#************************************
# Reading in data
#************************************
library(ggplot2)
library(ggpubr)
require(dplyr) 
library(ade4)
library(lattice) 
library("GGally")

# Set working directory
traindir <- "~/Desktop/Fall 2020/SYS4021/Data/TrainData"
sourcedir <-"~/Desktop/Fall 2020/SYS4021/Source"

setwd(sourcedir)
source("AccidentInput.R")

acts <- file.inputl(traindir)
totacts <- combine.data(acts)

#************************************
# Subset extreme data
#************************************
##Build a data frame with only extreme accidents for ACCDMG
dmgbox <-boxplot(totacts$ACCDMG)

ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

## Remove 9/11
xdmg <- xdmg[-186,]

## Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

# Setup categorical variables
xdmgnd$Type <- factor(xdmgnd$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

####################################
# Linear Model 1: ACCDMG~TEMP
####################################
library(MASS)
library(lindia)
accdmg.lm1<-lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)
df = read.csv("~/Desktop/Fall 2020/SYS4021/Source/housing.csv")
house.main <-lm(price~sqft + bedrooms + baths,data=df)
boxcox(house.main)

summary(accdmg.lm1)
coef(accdmg.lm1)

## Diagnostics Plot     
plot(accdmg.lm1,labels.id = NULL)

# Plot graphs individually
## Residual vs. Fitted
plot(accdmg.lm1,which=1)

## QQ
plot(accdmg.lm1,which=2) 

## Scale-Location
plot(accdmg.lm1,which=3) 

## Cook's distance
plot(accdmg.lm1,labels.id = NULL, which=4) 

p4<- gg_cooksd(accdmg.lm1, label = TRUE, show.threshold = TRUE,
               threshold = "convention", scale.factor = 0.5) +
  xlab("Obs. Number")+ylab("Cook's distance") +
  ggtitle("Cook's distance plot")+
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

p4

# Finding extreme distance points
xdmgnd[5337,]$INCDTNO2
xdmgnd[5337,]$TOTINJ
xdmgnd[5337,]$TYPE
which(xdmgnd$ACCDMG == max(xdmgnd$ACCDMG))

## Redisuals vs. Leverage
plot(accdmg.lm1,which=5) 

## Cook's dist vs. Leverage
plot(accdmg.lm1,which=6) 


## Do we violate the distributional assumption of our response variable?
# Box-Cox Transformation          
boxcox(accdmg.lm1) 

gg_boxcox(accdmg.lm1)

## Find y value for maximum lambda
max(boxcox(accdmg.lm1, plotit = F)$y)

## The best lambda and store in L
L<-boxcox(accdmg.lm1, plotit = F)$x[which.max(boxcox(accdmg.lm1, plotit = F)$y)] 

## The model with the best lambda transformation
accdmg.lm1.trans<-lm(ACCDMG^L~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)

# Plot transformed model diagnostic graphs individually
## Residual vs. Fitted
plot(accdmg.lm1.trans,which=1)

## QQ
plot(accdmg.lm1.trans,which=2) 

## Scale-Location
plot(accdmg.lm1.trans,which=3) 

## Cook's distance
plot(accdmg.lm1.trans,labels.id = NULL, which=4) 

gg_cooksd(accdmg.lm1.trans, label = TRUE, show.threshold = TRUE,
          threshold = "convention", scale.factor = 0.5) +
  xlab("Obs. Number")+ylab("Cook's distance") +
  ggtitle("Cook's distance plot")+
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))

####################################
# Linear Model 2: ACCDMG~TYPE
####################################
#accdmg.lm1<-lm(ACCDMG~TEMP+TRNSPD+CARS+HEADEND1,data=xdmgnd)

length(unique(xdmgnd$TYPE))

# Treat type to be a factor
Type_ACC <- as.factor(xdmgnd$TYPE)

# Lets look at the default treatment coding
contrasts(Type_ACC)

# Write a model to predict ACCDMG in terms of TYPE 
accdmg.lm2<-lm(ACCDMG~TYPE,data=xdmgnd)
summary(accdmg.lm2) 

tapply(as.numeric(xdmgnd$ACCDMG), as.factor(Type_ACC), mean)

####################################
# Linear Model 3: ACCDMG~DERAIL
####################################
table(xdmgnd$TYPE)

# Create a qualitative variable for derailments 
Derail <- rep(NA, nrow(xdmgnd))
Derail[which(xdmgnd$TYPE == 2)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 3)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 4)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 5)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 6)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 7)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 1)] <- "Yes DRL" 
Derail[which(xdmgnd$TYPE == 8)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 9)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 10)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 11)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 12)] <- "Not DRL" 
Derail[which(xdmgnd$TYPE == 13)] <- "Not DRL" 
table(Derail)

# This new variable, Derail, has to be a factor
Derail <- as.factor(Derail)

# Lets look at the default treatment coding
contrasts(Derail)

# Make model to guage if derailments increase the severity of accident damage
accdmg.lm3<-lm(ACCDMG~Derail,data=xdmgnd)
summary(accdmg.lm3) 

# Check if data supports results that derailments are less expensive than other types of accidents
xdmgnd$Derail_bin <- Derail
xdmgnd %>% group_by(Derail_bin)  %>% summarise(
  n = n(),
  mean_pr = mean(ACCDMG, na.rm=T))

####################################
# Stepwise Model
####################################
# Attach the dataset to the R search path. 
attach(xdmgnd) 

# Build a main effects + interaction model Derail, TRNSPD, TONS, CARS, and HEADEND1
accdmg.lm4<-lm(ACCDMG~(Derail + TRNSPD + TONS + CARS + HEADEND1)^2,data=xdmgnd)
summary(accdmg.lm4)

## Use stepwise regression on main effects + interaction model
accdmg.lm4.step <- step(accdmg.lm4)
summary(accdmg.lm4.step)

# Check if data supports question that high weight derailments are more than other types of accidents
ggplot(xdmgnd, aes(x=TONS, y=ACCDMG, color=Derail_bin)) + geom_point()

# Create a binary variable for high weight 
high_wgt <- rep(NA, nrow(xdmgnd))
mean_tons <- mean(xdmgnd$TONS, na.rm = TRUE) 
xdmgnd$high_wgt <- ifelse(xdmgnd$TONS>mean_tons, 1, 0)

xdmgnd %>% group_by(high_wgt, Derail_bin)  %>% summarise(
  n = n(),
  mean_pr = mean(ACCDMG, na.rm=T))

#Perform a Partial F Test on models
anova(accdmg.lm4,accdmg.lm4.step)
# P = 0.5228 > 0.05 => therefore use smaller model

#### Model Comparison
AIC(accdmg.lm4)
AIC(accdmg.lm4.step)

BIC(accdmg.lm4)
BIC(accdmg.lm4.step)

####################################
# Interactions
####################################
table(xdmgnd$TYPEQ)

# Create a qualitative variable for freight 
Freight <- rep(NA, nrow(xdmgnd))
Freight[which(xdmgnd$TYPEQ == 2)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 3)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 4)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 5)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 6)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 7)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 1)] <- "Yes Freight" 
Freight[which(xdmgnd$TYPEQ == 8)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 9)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 10)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 11)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 12)] <- "Not Freight" 
Freight[which(xdmgnd$TYPEQ == 13)] <- "Not Freight" 
table(Freight)

# This new variable, Derail, has to be a factor
Freight <- as.factor(Freight)

# Lets look at the default treatment coding
contrasts(Freight)

# Train Speed Factor
trnspdbox<-boxplot(xdmgnd$TRNSPD)
TRNSPD.factor<-xdmgnd$TRNSPD
TRNSPD.factor[which(xdmgnd$TRNSPD<median(xdmgnd$TRNSPD))]<-'low train speed'
TRNSPD.factor[which(xdmgnd$TRNSPD>=median(xdmgnd$TRNSPD))]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)

# Number cars Factor
CARSbox<-boxplot(xdmgnd$CARS)
CARS.factor<-xdmgnd$CARS
CARS.factor[which(xdmgnd$CARS<1)]<-'0 Cars'
CARS.factor[which(xdmgnd$CARS>=1)]<-'1+ Cars'
CARS.factor <- factor(CARS.factor)

# Create derailment variable
#Derail <- rep(0, nrow(xdmgnd))
#Derail[which(xdmgnd$TYPE == 1)] <- 1 
#Derail <- as.factor(Derail)
#contrasts(Derail)

# Create interaction plots
interaction.plot(TRNSPD.factor,CARS.factor, log(xdmgnd$ACCDMG))
interaction.plot(TRNSPD.factor,Freight, log(xdmgnd$ACCDMG))
interaction.plot(TRNSPD.factor,Derail, log(xdmgnd$ACCDMG))

