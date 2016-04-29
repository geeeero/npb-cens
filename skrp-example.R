# using SKPR data & expert knowledge for drive side ball bearing of the suction press

source("setup.R")

# field data: bearing lifetime in days
skrpfielddata <- read.csv("SKRP_DriveSideBearingSuctionPressDATA.csv", sep=";", header=FALSE,
                          colClasses = c("NULL", "numeric", "NULL", "factor"), skip=4)
names(skrpfielddata) <- c("time", "cens")
skrpfielddata$cens <- as.numeric(skrpfielddata$cens == "Uncensored")
skrpd <- data.frame(skrpfielddata, rank=rank(skrpfielddata$time))
qplot(data=skrpd, x=rank, ymin=rep(0,14), y=time, ymax=time, label=time, vjust=-0.5,
      geom=c("pointrange", "text"), shape=factor(cens)) + coord_flip() + scale_x_reverse() +
  scale_shape_manual(values=c(1, 19), label=c("No", "Yes"), name="Failure")

# expert info: bearing lifetime in years
skrpexpert <- read.csv("SKRP_DriveSideBearingSuctionPressEXPERT.csv", sep=";", header=FALSE,
                       colClasses = c("numeric", "numeric", "numeric", rep("NULL", 17)), skip=3, dec=",")
names(skrpexpert) <- c("timel", "timeu", "freq")

g <- ggplot(data=skrpexpert)
g + geom_bar(aes(x=timeu, y=freq), stat="identity")

skrpetimes <- (skrpexpert$timel + skrpexpert$timeu)/2
skrpe <- data.frame(Time=rep(skrpetimes, times=skrpexpert$freq))
ggplot(skrpe, aes(x=Time)) + geom_histogram(breaks=seq(0, 10, by=0.5))

# transform into days
skrpexpert$timel <- skrpexpert$timel * 365
skrpexpert$timeu <- skrpexpert$timeu * 365

g <- ggplot(data=skrpexpert, aes(x=timeu, y=1-cumsum(freq)/100))
g + geom_step(direction="vh") + geom_step(direction="hv")

# reliability values from freq
skrpexpert <- data.frame(skrpexpert, rel = 1-cumsum(skrpexpert$freq)/100)
skrpe2 <- data.frame(Time=c(0, skrpexpert$timeu), rell=c(skrpexpert$rel, 0), relu=c(1, skrpexpert$rel))
skrpe2 <- skrpe2[-21,]

# time grid interval: 18.25 days, such that 10 years = 200 time points
e3t <- seq(0, 3650, by=18.25)
e3nL <- data.frame(Bearing = rep(14, 201))
e3nU <- data.frame(Bearing = rep(100, 201))
e3yL <- data.frame(Bearing = c(rep(skrpe2$rell, each=10), 0))
e3yU <- data.frame(Bearing = c(rep(skrpe2$relu, each=10), 0.01))
#ggplot() + geom_line(aes(x=e3t, y=e3yL$Bearing)) + geom_line(aes(x=e3t, y=e3yU$Bearing))
e3yL <- not01(e3yL)
e3yU <- not01(e3yU)

# 'upper', optimistic data: censored bearings continue to function
uncens <- skrpfielddata$time[skrpfielddata$cens == 1]
udata <- c(uncens, rep(max(e3t)+1, dim(skrpfielddata)[1] - length(uncens)))
# 'lower', pessimistic data: censoring times are actually failure times
e3dL <- list(Bearing = skrpfielddata$time)
e3dU <- list(Bearing = udata)

e3resL <- oneCompPriorPostSet("Bearing", e3t, e3dL, e3nL, e3nU, e3yL, e3yU)
e3resU <- oneCompPriorPostSet("Bearing", e3t, e3dU, e3nL, e3nU, e3yL, e3yU)

e3df <- data.frame(      e3resL[,-match("Upper", names(e3resL))], # take out Upper from result with lower data
                   Upper=e3resU[, match("Upper", names(e3resU))]) # add Upper from result with upper data
priopostcolours1 <- scale_fill_manual(values = c(tuegreen, tuedarkblue))
priopostcolours2 <- scale_colour_manual(values = c(tuegreen, tuedarkblue))

e3plot <- ggplot(e3df, aes(x=Time)) + priopostcolours1 + priopostcolours2 
e3plot <- e3plot + geom_line(aes(y=Upper, group=Item, colour=Item)) + geom_line(aes(y=Lower, group=Item, colour=Item))
e3plot <- e3plot + geom_ribbon(aes(ymin=Lower, ymax=Upper, group=Item, colour=Item, fill=Item), alpha=0.5)
e3plot <- e3plot + xlab("Days") + ylab("Survival Probability") + bottomlegend + scale_x_continuous(breaks=(0:10)*365)
e3plot
#