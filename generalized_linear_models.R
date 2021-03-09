#********************************************************************************
#             
#				General Linear Models
#
#********************************************************************************

#************************************
# Reading in data
#************************************
library(ggplot2)
library(ggpubr)
library(ggbiplot)
require(dplyr) 
library(ade4)
library(lattice) 
library(ggfortify)
library(ggResidpanel)
library(MASS)
library("GGally")

# Set working directory
datadir <- "~/Desktop/Fall 2020 Classes/SYS4021/Data/Spam"
sourcedir <-"~/Desktop/Fall 2020 Classes/SYS4021/Source"

spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

#*****************************************
# Source code
#*****************************************
setwd(sourcedir)
source("AccidentInput.R")
source("SPM_Panel.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("pc.glm.R")
source("TestSet.R")

#**************************************************************
#
#				Graphical Analysis
#
#**************************************************************

# Look at the data
dim(spam)
summary(spam)

# Which variable is the response variable?
table(spam$V58)

# What proportion is spam?
sum(spam[,58])/length(spam[,58])
sum(spam[,58])
length(spam[,58])

#***************************************************************
#
#		Log of Predictors
#
#***************************************************************

# Repeat the above graphical analysis with a log transform of the
# predictor variables
Lspam <- log(spam[,-58] + .1)
Lspam[,58] <- spam[,58]

# Obtain boxplots with variables 1-9 vs. the response.
# Which variables are more discriminatory?
for(i in 1:9)
{
  assign(paste0("V", i), ggplot(data = Lspam, aes_string(x=as.factor(Lspam$V58),y=Lspam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}

ggarrange(V1,V2,V3,V4,V5,V6,V7,V8,V9,ncol=3,nrow=3)


#1. Obtain box plots for log transforms of variables 20-28 with variable 58.
for(i in 20:28)
{
  assign(paste0("V", i), ggplot(data = Lspam, aes_string(x=as.factor(Lspam$V58),y=Lspam[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}
ggarrange(V20,V21,V22,V23,V24,V25,V26,V27,V28,ncol=3,nrow=3)
dev.off()

#***************************************************************
#
#		Scatterplot matrix
#
#***************************************************************
uva.pairs(Lspam[,c("V58", "V49", "V50", "V51", "V52", "V53", "V54", "V55", "V56", "V57")])

#****************************************************
#
#		Principal Components
#
#****************************************************
# Obtain the principal components for variables 1-57. 
# Look at the biplot and explain what you see.
spam.pca = princomp(Lspam[,1:57], cor = T)
biplot(spam.pca)
ggbiplot(spam.pca, varname.size = 5, labels=row(spam)[,1])

cumplot(spam.pca, col = "grey")

biplot.fact <- function(pc.obj, res, comp = c(1,2), pch1 = 19, pch2 = 18)
{
  if ("scores" %in% names(pc.obj)){ # princomp object
    plot(pc.obj$scores[res == 0,comp[1]], pc.obj$scores[res == 0,comp[2]], col = "blue", xlab = paste("Comp", comp[1]), ylab = paste("Comp", comp[2]), ylim = c(min(pc.obj$scores[,comp[2]]), max(pc.obj$scores[,comp[2]])),
         xlim = c(min(pc.obj$scores[,comp[1]]), max(pc.obj$scores[,comp[1]])), pch = pch1)
    points(pc.obj$scores[res == 1,comp[1]], pc.obj$scores[res == 1,comp[2]], col = "red", pch = pch2)
  }else{ # prcomp object
    plot(pc.obj$x[res == 0,comp[1]], pc.obj$x[res == 0,comp[2]], col = "blue", xlab = paste("Comp", comp[1]), ylab = paste("Comp", comp[2]), ylim = c(min(pc.obj$x[,comp[2]]), max(pc.obj$x[,comp[2]])),
         xlim = c(min(pc.obj$x[,comp[1]]), max(pc.obj$x[,comp[1]])), pch = pch1)
    points(pc.obj$x[res == 1,comp[1]], pc.obj$x[res == 1,comp[2]], col = "red", pch = pch2)
  }
}

biplot.fact.gg <- function(pc.obj, res, labels, comp = c(1,2))
{
  if ("scores" %in% names(pc.obj)){ # princomp object
    ggplot(data = data.frame(pc.obj$scores), aes(x=pc.obj$scores[,comp[1]], y=pc.obj$scores[,comp[2]])) +
      geom_point(aes(color = factor(res))) + xlab("Comp 1") + ylab("Comp 2") + 
      theme(legend.title = element_blank()) + scale_color_discrete(labels=labels)
  }else{ # prcomp object
    ggplot(data = data.frame(pc.obj$x), aes(x=pc.obj$x[,comp[1]], y=pc.obj$x[,comp[2]])) +
      geom_point(aes(color = factor(res))) + xlab("Comp 1") + ylab("Comp 2") + 
      theme(legend.title = element_blank()) + scale_color_discrete(labels=labels)
  }
}

biplot.fact(spam.pca, Lspam[,58])
legend(-30, 10, legend = c("Spam", "Ham"), pch = c(18, 19), col = c("red", "blue"))

#****************************************************
#
#		General Linear Models
#
#****************************************************
#*

Lspam.short = Lspam[,c(1:10,47:58)]

#Creating main effects model with just the first 10 and last 10 (48:57) predictor variables each of which has been log transformed with a 0.1 offset
Lspam.glm.main <- glm(Lspam[,58]~., data = Lspam.short, family = binomial)
summary(Lspam.glm.main)
Lspam.null <- glm(Lspam[,58]~1, data = Lspam, family = binomial)
anova(Lspam.null, Lspam.glm.main, test = "Chi")

#Using drop1() function from the MASS library to compute  significance tests for each term in the main effects model using just the log transformed first and last 10 predictors
Lspam.glm.main <- glm(V58~., data = Lspam.short, family = binomial)
summary(Lspam.glm.main)
drop1(Lspam.glm.main, response~., test = "Chi", data = dspam)

spam.no4 <- update(Lspam.glm.main, .~.-V4, data = Lspam.short)
anova(spam.no4, Lspam.glm.main, test = "Chi")
(exp(Lspam.glm.main$coefficients[5])-1)*100

Lspam.short[1,]
predict(Lspam.glm.main, newdata = Lspam.short[1,])
exp(predict(Lspam.glm.main, newdata = Lspam.short[1,]))
exp(predict(Lspam.glm.main, newdata = data.frame(Lspam.short[1,], V4 = 1)))

#Predictions in the original dataset for main effects
spam.log.predict <- predict(Lspam.glm.main, type = "response")
score.table(spam.log.predict, spam[,58], 0.5)

#Stepwise
Lspam.glm.main <- glm(V58~., data = Lspam.short, family = binomial)
step.Lspam.glm.main <- step(Lspam.glm.main, data = Lspam.short, family = binomial)

#Partial likelihood test
anova(step.Lspam.glm.main, Lspam.glm.main, test = "Chi")

#Predictions in the original dataset for stepwise
spam.log.predict.step <- predict(step.Lspam.glm.main, type = "response")
score.table(spam.log.predict.step, spam[,58], 0.5)

#**************************************************
#
# 		Evaluate performance with AIC
#
#**************************************************

AIC(step.Lspam.glm.main)

AIC(Lspam.glm.main)

#*****************************
#
#   ROC Curves
#
#*****************************

roc.plot.gg <- plot.roc.gg(spam.log.predict, spam[,58], "Main Effects")
roc.plot.gg <- lines.roc.gg(roc.plot.gg, spam.log.predict.step, spam[,58], "Step")
roc.plot.gg

#****************************************************
#
#		GLM Principal Components Regression
#
#****************************************************
glm.spam.pca = princomp(Lspam.short, cor = T)
cumplot(glm.spam.pca, col = "grey")

spam.pca.glm98 <- pc.glm(glm.spam.pca, 98, spam[,58])
summary(spam.pca.glm98)

# Do a model utility test for model that uses 
# PCs that account for 98% of the variance
spampca.null <- pc.null(glm.spam.pca, 98, spam[,58])
anova(spampca.null, spam.pca.glm98, test = "Chi")

glm.spam.pca = princomp(Lspam.short, cor = T)
spam.pca.glm98 <- pc.glm(glm.spam.pca, 98, spam[,58])
glm.spam.pca.predict <- predict(spam.pca.glm98, type = "response")
score.table(glm.spam.pca.predict, spam[,58], 0.5)