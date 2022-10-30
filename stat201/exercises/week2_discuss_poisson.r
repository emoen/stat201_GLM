#Import the file deaths.txt: it includes the number of deaths by cancer occurred
#in Italy in 1999 and the population at risk (exposure), clustered by age and gender.


setwd("C:/prosjekt/uib/stat201/datasets")
dat <- read.table("deaths.txt")

##### PLOT ################
dba = aggregate(deaths ~ age, data=dat, FUN = sum)
allD = sum(dba$deaths)
dba$rates = dba$deaths/allD
plot(dba$age, dba$rates)

##### LINEAR AGE ##################
mod1 <- lm(deaths ~ age , data = dba)
summary(mod1)  
#R^2: 0.008, F-statistic: 0.23 on 1 and 29 DF,  p-value: 0.64

fitted <- predict(mod1) 

plot(dba$age, dba$deaths)
lines(dba$age, fitted)

# NORMAL PLOT - looks good
res <- rstandard(mod1) #Regression Deletion Diagnostics
qqnorm(res,ylim=c(-2,2))
abline(c(0,1))

#plot residuals to each x_i
plot(res) #dont look random

# Other diagnostics: 1. Leverage, 2. delta-beta = cook-distance 3. delta-deviance/cross-validation leave out i'th observation
influence(mod1) #This function provides the basic quantities which are used in forming a wide variety of diagnostics for checking the quality of regression fits.

# 1. Leverage: i'th element of H_hat. h_ii > 2*p/N, p=#parameters, N=#observations
param_obs = 2*2/nrow(dat)
h <- influence(mod1)$hat #
any(h>param_obs) == True

# 2.  delta-beta = cook-distance
cooks.distance # COOK distance
# None > 1

# 0. Standard residuals: plot them
# r_i = e_i / (sigma_hat* sqrt(1-h_ii)=
sigma <- sqrt(sum(res^2)/(nrow(dba)-2))
rr = res/(sigma * sqrt(1-h) ) #Looks similar to res

#RMSE
dba$predicted = as.integer(fitted)
RMSE = sqrt(mean((dba$deaths - dba$predicted)^2))
RMSPE = (sqrt(mean( ((dba$deaths - dba$predicted) / dba$deaths)^2 ) ) ) * 100
#RMSE = 1135 years, RMSPE=34.5%

##### LINEAR AGE + GENDER + EXPOSURE ##################
mod2 <- lm(deaths ~ age + gender + exposure , data = dat)
summary(mod2)
#R^2=0.52, F=23.37, p=4.8^-10

fitted2 <- predict(mod2) 

plot(dba$age, dba$deaths)
lines(dba$age, fitted2) #doesnt work - how to plot this?

# NORMAL PLOT - looks good
res2 <- rstandard(mod2)
qqnorm(res2,ylim=c(-2,2))
abline(c(0,1)) 

#RMSE
dat$predicted = as.integer(fitted2)
RMSE = sqrt(mean((dat$deaths - dat$predicted)^2))
RMSPE = (sqrt(mean( ((dat$deaths - dat$predicted) / dba$deaths)^2 ) ) ) * 100
# RMSE = 504 years, RMSPE = 17%

########### GLM #########################
poisson.model = glm(deaths ~ age + gender + exposure, dat, family=poisson(link="log"))
summary(poisson.model)
#AIC: 7333.3

fitted3 <- predict(poisson.model) 
dat$glm = as.integer(exp( fitted3 ) )

plot(dba$age, dba$deaths)
lines(dba$age, fitted3) #how to plot this?

# NORMAL PLOT - looks good
res2 <- rstandard(mod2)
qqnorm(res2,ylim=c(-2,2))
abline(c(0,1))

RMSE = sqrt(mean((dat$deaths - dat$glm)^2))
RMSPE = (sqrt(mean( ((dat$deaths - dat$glm) / dba$deaths)^2 ) ) ) * 100
# RMSE = 2165, RMSPE = 61%

# PLOT GLM prediction
range(dat$age)
xweight <- seq(60, 90, 2) #deaths
yweight = predict(poisson.model )
dat$glm = as.integer( exp( yweight ) )
plot(dat$glm[dat$gender=='F'], dat$deaths[dat$gender=='F']) #scatter
abline(c(0,1)) # y = x
######################################################
#misc
# 1. plot density
plot(dpois(dat$deaths[dat$gender=="M"],lambda_hat_female,log=F))

# 2. means
the_means = aggregate(dat$deaths, list(dat$gender), FUN=mean)

# 3. Numbers of chronic medical conditions reported contry/town
l0 <- sum(dpois(dat1$number,lambda_hat,log=T)) 

l1 <- sum(dpois(dat1$number[dat1$place=="town"],lambda_hat1,log=T))+
  sum(dpois(dat1$number[dat1$place=="country"],lambda_hat2,log=T)) 

# 4. P value ANOVA
model.anova = aov(deaths~age, data=dba)
summary(model.anova)

