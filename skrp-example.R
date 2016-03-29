# using SKPR data & expert knowledge for drive side ball bearing of the suction press

#install.packages("devtools")
#library("devtools")
#install_github("louisaslett/ReliabilityTheory")
library("ReliabilityTheory")
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(xtable)

setwd("../npb-cens")

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())

# produces survival signature matrix for one component of type "name",
# for use in nonParBayesSystemInference()
oneCompSurvSign <- function(name){
  res <- data.frame(name=c(0,1), Probability=c(0,1))
  names(res)[1] <- name
  res
}

# produces data frame with prior and posterior lower & upper component survival function
# for component of type "name" based on nonParBayesSystemInferencePriorSets() inputs
# for all components except survival signature; nLower, nUpper, yLower, yUpper must be data frames
# where each column corresponds to the component type, so there must be a match 
oneCompPriorPostSet <- function(name, at.times, test.data, nLower, nUpper, yLower, yUpper){
  sig <- oneCompSurvSign(name)
  nodata <- list(name=NULL)
  names(nodata) <- name
  nL <- nLower[, match(name, names(nLower))]
  nU <- nUpper[, match(name, names(nUpper))]
  yL <- yLower[, match(name, names(yLower))]
  yU <- yUpper[, match(name, names(yUpper))]
  data <- test.data[match(name, names(test.data))]
  prio <- nonParBayesSystemInferencePriorSets(at.times, sig, nodata, nL, nU, yL, yU)
  post <- nonParBayesSystemInferencePriorSets(at.times, sig,   data, nL, nU, yL, yU)
  data.frame(Time=rep(at.times,2), Lower=c(prio$lower,post$lower), Upper=c(prio$upper,post$upper),
             Item=rep(c("Prior", "Posterior"), each=length(at.times)))
}

# ----------------------------------------------

skrpfielddata <- read.csv("SKRP_DriveSideBearingSuctionPressDATA.csv", sep=";", header=FALSE,
                          colClasses = c("NULL", "numeric", "NULL", "factor"), skip=4)
names(skrpfielddata) <- c("time", "cens")
skrpfielddata$cens <- as.numeric(skrpfielddata$cens == "Uncensored")

skrpexpert <- read.csv("SKRP_DriveSideBearingSuctionPressEXPERT.csv", sep=";", header=FALSE,
                       colClasses = c("numeric", "numeric", "numeric", rep("NULL", 17)), skip=3, dec=",")
names(skrpexpert) <- c("timel", "timeu", "freq")

#