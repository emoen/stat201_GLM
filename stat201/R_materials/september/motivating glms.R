#########################################
# motivating glms with                  #
# examples of linear models limitations #
#########################################

# example with count data (DB chapter 3)

library(dobson)

dat <- mortality

dat$age <- seq(32,67,by=5)

dat$rate <- dat$deaths/dat$population
plot(dat$age,dat$rate)

# looks like we need an exponential curve to fit this data
# from demography we know that mortality rates follow
# rate = death/pop = a*exp(b*age) : this is the Gompertz law
# a naive approach to fit a Gompertz law: 
# take the log and fit a regression line
# log(rate) = log(a)+b*age

plot(dat$age,log(dat$rate))
slope <- cov(dat$age,log(dat$rate))/var(dat$age)
intercept <- mean(log(dat$rate))-slope*mean(dat$age)
abline(c(intercept,slope))

gompertz <- function(par, age=seq(30,70,by=0.01)){
  fun <- exp(par[1]+par[2]*age)
  return(fun)
}

plot(dat$age,dat$rate)
lines(seq(30,70,by=0.01),gompertz(c(intercept,slope)))

# this approach works if
# 1) death counts are all strictly positive 
# 2) the variance does not depend on the mean

# example with data in the form of proportion

# first watch
# https://www.youtube.com/watch?v=QlS7TKAiZcg

# roughly speaking, a run-up R is an elevation of the sea surface
# on the shore
# significant wave height H is a (robust) average of wave heights 
# within a space-time neighborhood

# find the runups datasets in mitt.no
setwd("C:/prosjekt/uib/stat201/datasets")
dat <- read.table("runups.txt")

# H is significant wave height in the open sea
# R2 is the proportion of runups higher than 2 meters

plot(dat)


# we want to fit a logistic curve
# R2 <- exp(a+b*H)/(1+exp(a+b*H)) 

# naive approach: we fit a regression line using
# log(R2/(1-R2))= a + b*H

plot(dat$H,log(dat$R2/(1-dat$R2)))
# expected problem: R2 can be 0 or 1
dat$R2
# naive (to be avoided) approach: ignore the data that don't fit with my approach
dat.naive <- dat[-c(1:2,16:24),]
plot(dat.naive$H,log(dat.naive$R2/(1-dat.naive$R2)))

slope <- cov(dat.naive$H,log(dat.naive$R2/(1-dat.naive$R2)))/var(dat.naive$H)
intercept <- mean(log(dat.naive$R2/(1-dat.naive$R2)))-slope*mean(dat.naive$H)
abline(c(intercept,slope))

logistic <- function(par, H=seq(0.2,2.5,by=0.1)){
  fun <- exp(par[1]+par[2]*H)/(1+exp(par[1]+par[2]*H))
  return(fun)
}
plot(dat.naive,xlim=c(0,3),ylim=c(0,1))

lines(seq(0.2,2.5,by=0.1),logistic(c(intercept,slope)))
points(dat,pch=3)
# the following is what we would obtain with an appropriate glm model
lines(seq(0.2,2.5,by=0.1),logistic(c(-7.029,6.451 )),col=2) 
# zoom to see the difference
