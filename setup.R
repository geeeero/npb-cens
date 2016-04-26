#install.packages("devtools")
#library("devtools")
#install_github("louisaslett/ReliabilityTheory")
library("ReliabilityTheory")
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(xtable)

#setwd("../npb-cens")

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())

tuered <- rgb(0.839,0.000,0.290)
tueblue <- rgb(0.000,0.400,0.800)
tueyellow <- rgb(1.000,0.867,0.000)

tuegreen <- rgb(0.000,0.675,0.510)
tuewarmred <- rgb(0.969,0.192,0.192)
tueorange <- rgb(1.000,0.604,0.000)
tuedarkblue <- rgb(0.063,0.063,0.451)

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

# setting lower and upper y^(0) values from 0 to 0 + eps and 1 to 1 - eps
# to avoid errors from nonParBayesSystemInferencePriorSets()
not01 <- function(arg, eps=1e-5){
  arg[arg == 0] <- eps
  arg[arg == 1] <- 1 - eps
  arg
}


#