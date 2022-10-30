######################
# examples chapter 7 #
######################

library(dobson)
data(beetle)
dat <- beetle

plot(dat$x,dat$y/dat$n)

#################
# probit model  #
#################
probit_reg <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "probit")) 
summary(probit_reg)

# the glm output includes:
# summary statistics of the deviance residuals
summary(residuals(probit_reg))  # deviance residuals

sum(residuals(probit_reg)^2) # residual deviance (the deviance in DB book)

# deviance  components
D_null <- probit_reg$null.deviance # deviance from the null model
D_res <- probit_reg$deviance # deviance from the fitted model


# goodness of fit
# p-value of D_res
1-pchisq(D_res,8-2) # not a very good fit
# compare D_res = 10.12  to
qchisq(0.95,6)
# note that n = 8 

# null model
probit_reg0 <- glm(cbind(y,n-y) ~ 1, data = dat, family = binomial(link = "probit"))
summary(probit_reg0)

x <- seq(min(dat$x),max(dat$x),by=0.01)
probit_pred <- predict(probit_reg, newdata = data.frame(x = x) ,type = "response")
lines(x,probit_pred)

# coefficients interpretation
# the predictor is equal to 0 when
# x = 34.935 /19.728 = -beta0/beta1
34.935 /19.728
abline(v = 34.935 /19.728)
abline(h = 0.5)
# pi < 0.5 for x < 1.77
# pi > 0.5 for x > 1.77
# 1.77 is often referred to as the median dose

###############
# logit model #
###############
logit_reg <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "logit")) 
summary(logit_reg)
logit_pred <- predict(logit_reg, newdata = data.frame(x = x) ,type = "response")
lines(x,logit_pred,lty=2)
1-pchisq(logit_reg$deviance,8-2) # slightly worse than probit

# interpretation of the parameters
x0 <- 60.717 /34.270
plot(dat$x,dat$y/dat$n)
lines(x,logit_pred,lty=2)
abline(v = x0)
abline(h = 0.5)

# complementary log-log
cloglog_reg <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "cloglog")) 
summary(cloglog_reg)
cloglog_pred <- predict(cloglog_reg, newdata = data.frame(x = x) ,type = "response")
lines(x,cloglog_pred,lty=3)

# parameter interpretation
x0 <- 39.572/22.041
abline(v=x0,lty=3)
abline(h = 1-exp(-1),lty=3)

# comparing models
probit_reg$deviance
logit_reg$deviance
cloglog_reg$deviance
1-pchisq(cloglog_reg$deviance,8-2) # best model among the models we tried

# AIC confirms cloglog as the best model
probit_reg$aic
logit_reg$aic
cloglog_reg$aic


# changing the definition of the dependent variable
probit_reg1 <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "probit")) 
probit_reg2 <- glm(cbind(n-y,y) ~ x, data = dat, family = binomial(link = "probit")) 
coef(probit_reg1)
coef(probit_reg2)

logit_reg1 <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "logit")) 
logit_reg2 <- glm(cbind(n-y,y) ~ x, data = dat, family = binomial(link = "logit")) 
coef(logit_reg1)
coef(logit_reg2)

# now watch!
cloglog_reg1 <- glm(cbind(y,n-y) ~ x, data = dat, family = binomial(link = "cloglog")) 
cloglog_reg2 <- glm(cbind(n-y,y) ~ x, data = dat, family = binomial(link = "cloglog")) 
coef(cloglog_reg1)
coef(cloglog_reg2)
