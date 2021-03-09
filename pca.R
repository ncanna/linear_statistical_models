#********************************************************************************
#             
#				Principal Component Analysis
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
totacts$Casualty = totacts$TOTKLD + totacts$TOTINJ

#Boxplots of casualties damage by year
ggplot(data = totacts, aes(y = YEAR4, x = Casualty)) +
  geom_boxplot() + 
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plot of Total Casualties") +
  labs(x = "Casualties", y = "Year")

b <- boxplot(totacts$Casualty)
b$stats

#************************************
# Exploratory Analysis
#************************************

#Proportion accidents with one or more casualties
(dim(totacts[totacts$Casualty >= 1, ])[1] * 100)/dim(totacts)[1]

#How many accident reports over all 19 years have at least one casualty?
dim(totacts[totacts$Casualty >= 1, ])[1]

#Creating a data frame containing only accidents with one or more casualties. Use the variables "INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN" to determine if there are duplicates in the accident reports with one or more casualties.
casualties_df <- totacts[totacts$Casualty >= 1, ]
dim(casualties_df)[1]
summary(duplicated(casualties_df[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))

#Removing the duplicated reports from the data frame with accidents containing one or more casualties.
casualties_df_nd <- casualties_df[!duplicated(casualties_df[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]), ]
dim(casualties_df_nd)[1]

#Getting the 2nd largest number of casualties in a single accident per year
casualtiesbygroups <- aggregate(casualties_df_nd$Casualty, by=list(Category=casualties_df_nd$YEAR4), FUN=sum)
casualtiesbygroups[order(-casualtiesbygroups$x),][2,]

#What year (of those listed below) has the lowest total number (sum) of casualties?
casualtiesbygroups[order(casualtiesbygroups$x),][1,]

#Which year (of those listed below) has the fewest number of accidents with casualties?
summary(casualties_df_nd$YEAR4)
aggregate(ClaimDay~day,FUN=length,data=mydata)
num_per_year <- casualties_df_nd %>% count(YEAR4)
num_per_year[order(-num_per_year$n),]

#Which accident type has the largest number of accidents with one or more casualties?
num_per_type <- casualties_df_nd %>% count(TYPE)
num_per_type[order(-num_per_type$n),]

#Which Cause (see the definition of this variable in S3MultivarViz.R) has the largest number of accidents with one or more casualties?
casualties_df_nd$Cause <- rep(NA, nrow(casualties_df_nd))
casualties_df_nd$Cause[which(substr(casualties_df_nd$CAUSE, 1, 1) == "M")] <- "M"
casualties_df_nd$Cause[which(substr(casualties_df_nd$CAUSE, 1, 1) == "T")] <- "T"
casualties_df_nd$Cause[which(substr(casualties_df_nd$CAUSE, 1, 1) == "S")] <- "S"
casualties_df_nd$Cause[which(substr(casualties_df_nd$CAUSE, 1, 1) == "H")] <- "H"
casualties_df_nd$Cause[which(substr(casualties_df_nd$CAUSE, 1, 1) == "E")] <- "E"

num_per_cause <- casualties_df_nd %>% count(Cause)
num_per_cause[order(-num_per_cause$n),]

#Which Cause has the accident with the largest number of casualties?
max_casualties <- casualties_df_nd[which.max(casualties_df_nd$Casualty),]
max_casualties$CAUSE

#Which type of train (TYPEQ) has the largest number of accidents with one or more casualties?
num_per_typeq <- casualties_df_nd %>% count(TYPEQ)
num_per_typeq[order(-num_per_typeq$n),]

#What is the total number (sum) of casualties over all 19 years discluding duplicates?
sum(casualtiesbygroups[order(-casualtiesbygroups$x),]$x)

#Remove the accident with the most extreme number of casualties and plot the scatter plot matrix of Casualty with "TRNSPD", "CARS", "TIMEHR", "TEMP"
nonextremecasualties <- casualties_df_nd[-which.max(casualties_df_nd$Casualty),]
nonextremecasualties[which.max(nonextremecasualties$Casualty),]
source("SPM_Panel.R")
uva.pairs(nonextremecasualties[,c("TRNSPD", "CARS", "TIMEHR", "TEMP")])

#Perform principal component analysis on the data (de-duplicated accident reports with one or more casualties and the accident with the most casualties removed) for the variables "Casualty", "TRNSPD", "CARS", "TIMEHR", "TEMP" using the correlation matrix.
pca_df <- nonextremecasualties[,c("TRNSPD", "CARS", "TIMEHR", "TEMP")]
pca_df.corr <- princomp(pca_df, cor=T)

barplot(pca_df.corr$loadings[,1], main='PC1 Loadings with Correlation Matrix')
barplot(pca_df.corr$loadings[,2], main='PC2 Loadings with Correlation Matrix')

#Which best approximates the amount of variance in the first two principal components?
cumplot <- function(pca.obj, ...)
{
  xc <- cumsum(pca.obj$sdev^2)/sum(pca.obj$sdev^2)
  barplot(xc, ylim = c(0,1), main = "Proportion of Variance", ylab = "Proportion", names.arg = 1:length(pca.obj$sdev), xlab = "Components", ...)
  xc <- as.data.frame(xc)
  setDT(xc, keep.rownames=TRUE)[]
  names(xc)[names(xc) == "rn"] <- "Component"
  names(xc)[names(xc) == "xc"] <- "Proportion"
  return(xc)
}
cumplot(pca_df.corr, col = "grey")




