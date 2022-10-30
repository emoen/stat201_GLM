############################
# constant hazard and      #
# Kaplan - Meier estimator #
############################

library(flexsurv)
library(MASS)
library(car)

# simulating fixed right-censoring (e.g., due to end of the study)
set.seed(345)
n = 500
x <- rexp(n, rate = 0.1)
hist(x)

C = 15 # end of study
c <- rep(C,n)
t <- apply(cbind(x,c),1,min)
delta <- ifelse(c > t, 1, 0)
dat <- as.data.frame(cbind(x,c,t,delta))
# take a look at dat and make sure you understand its content

hist(t,xlim=c(0,max(x))) # this histogram is misleading: censored data are treated as missing

#the first ten observations
plot(t[1:10],seq(1/10,1,by=1/10), pch=21*delta, yaxt="n",ylab = "",bg=1 )
segments(rep(0,10),seq(1/10,1,by=1/10),t[1:10],seq(1/10,1,by=1/10))

# create a survival object
surv.data <- Surv(t,delta)
surv.data[1:10] #  "+" indicates censoring

# MLE of rate lambda
mod <- flexsurvreg(surv.data ~ 1 , dist = "exp")
mod
lambda.hat <- exp(mod$coefficients)
lambda.hat # compare mean(delta)/mean(t)
lambda.se <- sqrt(mean(delta)/(n*mean(t)^2))
lambda.se

#total time at risk
sum(t)
#log-likelihood
loglik <- log(lambda.hat)*sum(delta)-lambda.hat*sum(t)
loglik
#AIC
-2*loglik+2*1


# what if we exclude censored values?
1/mean(t*delta)
# what if we pretend that censored times relate to events?
1/mean(t)
# in both cases we get biased estimates



# for some functions of the parameters estimation is immediate
# (for the moment, ignore the nonparametric estimates - black lines)
plot(mod, type = "survival")
plot(mod, type = "cumhaz")



##################################
# italian semi-supercentenarians #
##################################

# Italian females born in 1904 
# age at entry and exit is in days
# left truncated right censored data
dat <- read.table("female_cohort_1904.txt")

mod <- flexsurvreg(Surv(age.at.entry/365.25-105,age.at.exit/365.25-105,status) ~ 1, data= dat, dist = "exp")
mod
plot(mod,type="survival")
# estimate death probability 
# q = 1-S(x+1)/S(x) = 1 - exp(-rate)
1-exp(-0.6238533)

# Kaplan - Meier

####################
# fan failure data #
####################

# data from Nelson (1969) Hazard plotting for incomplete failure data. 
# J. Quality Technology, 1:27-52, 1969. 

# engineering study of the time to failure of diesel generator fans
# the problem was to determine whether the failure rate was decreasing over time
# failure_time is number of hours of running time up to failure or end of the study
# (whichever came first)

dat <- read.table("fan_failure.txt")

# Kaplan-Meier estimate of the survival function
Surv(dat$failure.time, dat$status)

KM <- survfit(Surv(failure.time, status) ~ 1, data = dat)
summary(KM)
summary(KM)$surv # returns the Kaplan-Meier estimate at each t_i
summary(KM)$time # {t_i}
summary(KM)$n.risk # {r_i}
summary(KM)$n.event # {d_i}
summary(KM)$std.err # standard error of the K-M estimate at {t_i}
summary(KM)$lower # lower pointwise estimates (option conf.int changes the ci level)
summary(KM)$upper # lower pointwise estimates

# let's check "manually" ...
lambdas <- summary(KM)$n.event/summary(KM)$n.risk
manual_KM <- cumprod(1-lambdas)
manual_KM
summary(KM)$surv


# plotting the the KM estimator
plot(KM)
plot(KM, mark.time=TRUE)

lines(KM,conf.int=0.99,col=2) # another ci level 



#############################
# comparing nonpar and par  #
#############################

# fan failure data

dat <- read.table("fan_failure.txt")
mod_exp <- flexsurvreg(Surv(failure.time, status) ~ 1, data = dat, dist = "exp")
plot(mod_exp, type="cumhaz")
plot(mod_exp, type="survival")

# Italian semi-supercentenarians

dat <- read.table("female_cohort_1904.txt")
mod_exp <- flexsurvreg(Surv(age.at.entry/365.25-105,age.at.exit/365.25-105,status) ~ 1, data= dat, dist = "exp")
plot(mod_exp, type="cumhaz")
plot(mod_exp, type="survival")

