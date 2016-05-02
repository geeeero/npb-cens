# what are Surv objects?

library(survival)
?Surv

test <- Surv(skrpd$time, skrpd$cens)
test
attributes(test)

test2 <- unclass(test)
test2
# matrix with first column time and second column event indicator (0 = censored, 1 = failed)
# and attribute "type" = "right".
# column names are "time" and "status"

test3 <- sort(test) # seems to sort Surv objects properly
unclass(test3)
#