#Import the file deaths.txt: it includes the number of deaths by cancer occurred
#in Italy in 1999 and the population at risk (exposure), clustered by age and gender.


setwd("C:/prosjekt/uib/stat201/datasets")
dat <- read.table("deaths.txt")

#a) Plot death rates by age and interpret the pattern you see
dba = aggregate(deaths ~ age, data=dat, FUN = sum)
allD = sum(dba$deaths)
dba$rates = dba$deaths/allD
plot(dba$age, dba$rates)


#Obtain the least square estimates of the parameters of the model
#model1 log rate = β0 + β1age
#and show the Least Squares Estimate with an appropriate picture

mod1 <- lm(deaths ~ age , data = dba)
summary(mod1) # the main output

fitted <- predict(mod1) 

plot(dba$age, dba$deaths)
lines(dba$age, fitted)

#c) Q:Do you think that the model1 is acceptable for these data? Why?
#A: No, the data is not linear. R^2=0.0078,  bad fit
# leverage, Cook distance, delta-beta, 

# standardized residuals
res <- rstandard(mod1) #Regression Deletion Diagnostics
qqnorm(res,ylim=c(-2,2))
abline(c(0,1))
# residuals look normally distributed!

# provides several diagnostics (leverages, dfbetas, sigmas and residuals)
influence(mod1) #This function provides the basic quantities which are used in forming a wide variety of diagnostics for checking the quality of regression fits.
h <- influence(mod1)$hat
sigma <- sqrt(sum(res^2)/(nrow(dba)-2))
cooks.distance(mod1)

model.anova = aov(deaths~age, data=dba)
summary(model.anova)

#RMSE
dba$predicted = as.integer(fitted)
#sapply(dba, class)
RMSE = sqrt(mean((dba$deaths - dba$predicted)^2))
RMSPE = (sqrt(mean( ((dba$deaths - dba$predicted) / dba$deaths)^2 ) ) ) * 100
#RMSE = 1135 years, ans RMSPE=34.5% which is huge

# d) Are you able to suggest an alternative model that is better than model1?
#    Motivate your answer with an analysis similar to that done in examples chapter 2.R
# Deaths - Poisson distribution
mod2 <- lm(deaths ~ age + gender + exposure , data = dat)
summary(mod2)
#R^2=0.52, F=23.37, p=4.8^-10

fitted2 <- predict(mod2) 

plot(dba$age, dba$deaths)
lines(dba$age, fitted2)

res2 <- rstandard(mod2) #Regression Deletion Diagnostics
qqnorm(res2,ylim=c(-2,2))
abline(c(0,1))

#RMSE
dat$predicted = as.integer(fitted2)
#sapply(dba, class)
RMSE = sqrt(mean((dat$deaths - dat$predicted)^2))
RMSPE = (sqrt(mean( ((dat$deaths - dat$predicted) / dba$deaths)^2 ) ) ) * 100
# RMSE = 504, RMSPE = 17%


# sum of squared residuals is a chi-square
###################
# (standardized) residuals under H0
lambda_hat <- mean(dat$deaths)
res0 <- (dat$deaths - lambda_hat)/sqrt(lambda_hat)
sqrt(sum(res^2)/(nrow(dat)-1))
##################
chi0 <- sum(res0^2)
1-pchisq(chi0,nrow(dat)-1) # value consistent with H0
1-pchisq(sum(res2),nrow(dat)-1)
# also chi0 is close to the mean of the chi-square (48)
# X^2 = sum( (o_i - e_i)^2 / e_i) ~ X^2(m) where o_i = Y_i, e_i = theta_i, so sum(r^2) ~ X^2(m)
#######################################################
# From chapter 2 
mean = mean(dat$deaths)
variance = var(dat$deaths)

poisson.model = glm(deaths ~ age + gender + exposure, dat, family=poisson(link="log"))

fitted3 <- predict(poisson.model) 
dat$predicted = as.integer(fitted3)
#sapply(dba, class)
RMSE = sqrt(mean((dat$deaths - dat$predicted)^2))
RMSPE = (sqrt(mean( ((dat$deaths - dat$predicted) / dba$deaths)^2 ) ) ) * 100
# RMSE = 2165, RMSPE = 61%

the_means = aggregate(dat$deaths, list(dat$gender), FUN=mean)
lambda_hat_female = the_means[1,2]
lambda_hat_male   = the_means[2,2]
  
plot(dpois(dat$deaths[dat$gender=="M"],lambda_hat_female,log=F))
#dpois gives the (log) density
l1 <- sum(dpois(dat$deaths[dat$gender=="F"],lambda_hat_female,log=F))+
  sum(dpois(dat1$deaths[dat$gender=="M"],lambda_hat_male,log=T)) 