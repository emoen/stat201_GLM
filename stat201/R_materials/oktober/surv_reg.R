#################################
# PH and AFT regression models  #
#################################

library(flexsurv) # loads the survival package too
library(dobson)

# in survreg, the Weibull parametrization is different of the
# parameterization of R
# from ?rweibull we see
# f(x) = (a/b) (x/b)^(a-1) exp(- (x/b)^a)
# a = shape and b = scale
y <- rweibull(1000, shape=2, scale=5)
mod0 <- survreg(Surv(y)~1, dist="weibull")
mod0
# survreg's scale  =    1/(rweibull shape)
1/mod0$scale
#   survreg's intercept = log(rweibull scale)
exp(coef(mod0))

# simulate from two Weibull pdfs
# n <- 200
set.seed(123)
y0 <- rweibull(500, shape=2, scale=5)
y1 <- rweibull(500, shape=2, scale=8)
beta <- log(8)-log(5)  
beta # positive, subjects live more
y <- c(y0,y1)
x <- rep(0:1, each = 500)
mod <- survreg(Surv(y) ~ x, dist = "weibull") 
mod
# survreg's scale  =    1/(rweibull shape)
1/mod$scale
#   survreg's intercept = log(rweibull scale)
exp(coef(mod)[1])
coef(mod)[2]
time <- seq(0,25,by=0.01)
plot(time, 1-pweibull(time, shape=1/mod$scale, scale=exp(coef(mod)[1])),type="l",ylab="survival")
af <- exp(-coef(mod)[2]) # acceleration factor
af
lines(time, 1-pweibull(af*time,shape=1/mod$scale, scale=exp(coef(mod)[1])),col=2)
abline(h=0.5)

# AFT MODELS

###################### 
# breast cancer data #
######################
data(bc)
?bc
summary(bc)

# KM estimates, stratified by group
KM <- survfit(Surv(recyrs,censrec) ~  group, data = bc)
plot(KM,col=1:3)
legend("bottomleft",lty=1, legend=levels(bc$group),col=1:3)


mod_weibull <- survreg(Surv(recyrs, censrec) ~ group, data = bc,dist = "weibull")
mod_weibull

time <- seq(0,10,by=0.01)
lines(time, 1-pweibull(time, shape=1/mod_weibull$scale, scale=exp(coef(mod_weibull)[1])),ylab="survival")
af1 <- exp(-coef(mod_weibull)[2]) # acceleration factor Medium group
lines(time, 1-pweibull(af1*time,shape=1/mod_weibull$scale, scale=exp(coef(mod_weibull)[1])),col=2)
af2 <- exp(-coef(mod_weibull)[3]) # acceleration factor Poor group
lines(time, 1-pweibull(af2*time,shape=1/mod_weibull$scale, scale=exp(coef(mod_weibull)[1])),col=3)

# PH MODELS

# for fitting PH models we use the flexsurvreg function of the
# package flexsurv

dat <- read.table("italy_cohort_1890.txt",sep="\t", header = T)
summary(dat)
dat$sex <- as.factor(ifelse(dat$sex==1,"male","women"))
#subset oldest subjects
dat <- dat[dat$ages>60,]

KM <- survfit(Surv(ages-60,status) ~  sex, data = dat)
plot(KM,lty=1:2)

mod_exp <- flexsurvreg(Surv(ages-60,status) ~  sex, data = dat,dist = "exp")
print(mod_exp)
lines(mod_exp,col=2,lty=2:1)

mod_weibullPH <- flexsurvreg(Surv(ages-60,status) ~  sex, data = dat,dist = "weibullPH")
print(mod_weibullPH)
lines(mod_weibullPH,col=4,lty=2:1)

mod_gompertz <- flexsurvreg(Surv(ages-60,status) ~  sex, data = dat,dist = "gompertz")
print(mod_gompertz)
lines(mod_gompertz,col=5)



# cumulative hazards
plot(mod_weibullPH, type = "cumhaz")
lines(mod_gompertz, type = "cumhaz", col=4 )

mod_weibullPH$AIC
mod_gompertz$AIC


