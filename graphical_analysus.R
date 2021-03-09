#********************************************************************************
#             
#					Graphical Analysis and Visualization 
#
#********************************************************************************

#************************************
# Reading in data
#************************************
# Set working directory
traindir <- "~/Desktop/Fall 2020/SYS4021/Data/TrainData"
sourcedir <-"~/Desktop/Fall 2020/SYS4021/Source"

setwd(traindir)

acts19 <- read.csv("RailAccidents19.csv")

summary(acts19[,c("ACCDMG", "TOTKLD", "CARS")])

#************************************
# Measurement scales
#************************************
unique(acts19$ACCDMG)

names(acts19)

unique(acts19$CAUSE)

unique(acts19$TEMP)

#************************************
# Concatenating data
#************************************

setwd(sourcedir)
source("AccidentInput.R")

setwd(traindir)

my.files <- list.files(traindir)

acts <- lapply(my.files, read.csv)

acts <- file.inputl(traindir) 

#************************************
# Data Cleaning
#************************************

# Are the number of columns different from year to year?
ncol(acts[[1]])
ncol(acts[[8]])
ncol(acts[[7]])

# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# Now combine the data frames for all 19 years
# Use combine.data()
totacts <- combine.data(acts, comvar)

dim(totacts)

#************************************
# Histograms and Box Plots
#************************************
library(ggplot2)
library(ggpubr)

hist(totacts$HIGHSPD)

hist(totacts$TYPE)

hist(totacts$CAUSE)

barplot(rev(sort(table(totacts$CAUSE)))[1:5], main = "Causes", xlab = "column_data", ylab = "Frequency",col='coral')

ggplot(as.data.frame(totacts$HIGHSPD), aes(x=totacts$HIGHSPD)) + geom_histogram(fill= "steelblue", binwidth = 10) + ggtitle("Frequency of Speed") + labs(x = "Speed (MPH)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

#Boxplots of accident damage by year
for(i in 1:17)
{
  j <- which(colnames(acts[[11]]) == "ACCDMG")
  if(i < 10)
  {
    assign(paste0("AccBC", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("200", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip())+ xlim(c(0,1.7e7)) 
  }
  else
  {
    assign(paste0("AccBC", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("20", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()) + xlim(c(0,1.7e7)) 
  }
  
}

ggarrange(AccBC1,AccBC2,AccBC3,AccBC4,AccBC5,AccBC6,AccBC7,AccBC8,AccBC9,AccBC10,AccBC11,AccBC12,AccBC13,AccBC14,AccBC15,AccBC16,AccBC17, ncol = 3, nrow = 6)

ggplot(data = totacts, aes(y = YEAR, x = EQPDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Equipment Damage") +
  labs(x = "Year", y = "Damage")

#Line plots for trends
EQPDMG = c()
for(i in 1:19)
{
  EQPDMG[i] <- totacts$EQPDMG[which(totacts$EQPDMG == max(totacts$EQPDMG[which(totacts$IYR==i)]))]
}
plot(EQPDMG,type = "o")


TOTKLD = c()
for(i in 1:19)
{
  TOTKLD[i] <- totacts$TOTKLD[which(totacts$TOTKLD == max(totacts$TOTKLD[which(totacts$IYR==i)]))]
}
plot(TOTKLD,type = "o")

TRKDMG = c()
for(i in 1:19)
{
  TRKDMG[i] <- totacts$TRKDMG[which(totacts$TRKDMG == max(totacts$TRKDMG[which(totacts$IYR==i)]))]
}
plot(TRKDMG,type = "o")

TOTINJ = c()
for(i in 1:19)
{
  TOTINJ[i] <- totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ[which(totacts$IYR==i)]))]
}
plot(TOTINJ,type = "o")

totacts[which(totacts$TOTINJ == totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]) ,]$INCDTNO 
totacts[which(totacts$TOTINJ == totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]) ,]

CARSDMG = c()
for(i in 1:19)
{
  CARSDMG[i] <- totacts$CARSDMG[which(totacts$CARSDMG == max(totacts$CARSDMG[which(totacts$IYR==i)]))]
}
plot(CARSDMG,type = "o")

hist(totacts$TEMP, bins = nclass.Sturges(totacts$TEMP))

ACCDMG = c()
for(i in 1:19)
{
  ACCDMG[i] <- totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG[which(totacts$IYR==i)]))]
}
plot(ACCDMG,type = "o")

#**************************************************
#
#			Scatter Plot Matrices
#
#**************************************************

# Scatter plots
#tapply aplpies function for each cell of a vector array
df <- data.frame(year=2001:2019,damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))
ggplot(data=df, aes(x=year, y=damages)) + geom_line() + geom_point()

setwd(sourcedir)

source("SPM_Panel.R")
library(GGally)

# without panel functions for 2019
pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts[[19]])

# with panel function- a little more detail
ggpairs(acts[[19]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])


# Do this for all accidents
ggpairs(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

library(lattice)

uva.pairs(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

#**************************************************
#
#			EDA and Extreme Values
#
#**************************************************

which(totacts$ACCDMG == max(totacts$ACCDMG))
totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG))]
which(totacts$ACCDMG == 31538754)

totacts[47908,]$YEAR 
totacts[47965,]$YEAR 
totacts[47908,]$TYPE 
totacts[47965,]$TYPE 
totacts[47908,]$TOTINJ 
totacts[47965,]$TOTINJ
totacts[47908,]$TOTKLD 
totacts[47965,]$TOTKLD 
totacts[47908,]
totacts[47965,]

which(totacts$TOTKLD == max(totacts$TOTKLD))
which(totacts$TOTKLD == totacts$TOTKLD[which(totacts$TOTKLD == max(totacts$TOTKLD))]) 
totacts[29219,]$TOTKLD

totacts[which(totacts$TOTINJ == totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]) ,]$TOTINJ

totacts[which(totacts$TOTINJ == totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]) ,]$INCDTNO

which(totacts$ACCDMG > 1500000)

which(totacts$TOTKLD >= 1)

table(totacts$TYPE)

table(totacts$CAUSE)

