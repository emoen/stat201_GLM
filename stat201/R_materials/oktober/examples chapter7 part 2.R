###################################
# examples in chapter 7 - part II #
###################################

#################
# data senility #
#################

library(dobson)
data("senility")
dat <- senility
?senility

# we transform the data format in terms of covariate patterns 
y <- tapply(dat$s,as.factor(dat$x),sum)
x <- as.numeric(names(table(dat$x)))
n <- table(dat$x)
dat <- data.frame(cbind(y,n,x))
plot(dat$x,dat$y/dat$n)
logit_reg <- glm(cbind(y,n-y) ~ x , data = dat, family = binomial(link = "logit"))
summary(logit_reg)

N <- length(table(dat$x))

1-pchisq(logit_reg$deviance, N-2)
# not bad, but not excellent

xx <- seq(min(x),max(x),by=0.01)
pred <- predict(logit_reg, newdata = data.frame(x=xx), type="response")
lines(xx,pred)


# the Hosmer-Lemenshow test

# cluster predictions in g groups (e.g. g=3)
sum(dat$n)/3
cumsum(dat$n)
dat$group <- as.factor(c(rep(1,6),rep(2,4),rep(3,7)))
dat$pred <- predict(logit_reg, type="response")
exp1 <- tapply(dat$pred*dat$n,dat$group,sum)
obs1 <- tapply(dat$y,dat$group,sum)
plot(obs1,exp1)
abline(c(0,1))
exp0 <- tapply(dat$n - dat$pred*dat$n,dat$group,sum)
obs0 <- tapply(dat$n - dat$y,dat$group,sum)
plot(obs0,exp0)
abline(c(0,1))
# no outliers
HL_test <- sum((obs1-exp1)^2/exp1)+sum((obs0-exp0)^2/exp0)
HL_test 

g <- 3

1-pchisq(HL_test, g - 2)
# the model seems acceptable

################
# data anthers #
################

data(anthers)
dat <- anthers

plot(dat$centrifuge, dat$y/dat$n, pch = 21, bg=dat$storage, ylim=c(0.40,0.80))
legend(x= "topright", pch = rep(21,2), pt.bg=1:2, legend=c("control","treatment"))
# an ANCOVA analysis
dat$f.storage <- as.factor(dat$storage) #or, equivalently, dat$storage <- dat$storage-1
dat$log.centrifuge <- log(dat$centrifuge) # as suggested by the book

#different intercepts and slopes
mod1 <- glm(cbind(y, n-y) ~ f.storage + log.centrifuge  + f.storage:log.centrifuge, data = dat, family = binomial(link="logit"))
summary(mod1)

mod <- mod1
xx <- seq(min(dat$centrifuge), max(dat$centrifuge), length=100)
pred1 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(1, length(xx)))),type="response")
lines(xx,pred1)
pred2 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(2, length(xx)))),type="response")
lines(xx,pred2,col=2)

# different intercept but same slope
mod2 <- glm(cbind(y, n-y) ~ f.storage + log.centrifuge, data = dat, family = binomial(link="logit"))
summary(mod2)

mod <- mod2
pred1 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(1, length(xx)))),type="response")
lines(xx,pred1,lty=2)
pred2 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(2, length(xx)))),type="response")
lines(xx,pred2,col=2,lty=2)

# same intercept and slope
mod3 <- glm(cbind(y, n-y) ~ log.centrifuge, data = dat, family = binomial(link="logit"))
summary(mod3)

mod <- mod3
pred1 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(1, length(xx)))),type="response")
lines(xx,pred1,lty=3)
pred2 <- predict(mod, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(2, length(xx)))),type="response")
lines(xx,pred2,col=2,lty=3)


anova(mod3, mod2)       
1-pchisq(5.4727,1)
# we reject the null hypothesis of equal intercept

# regression bands (an example with model 1)
plot(dat$centrifuge, dat$y/dat$n, pch = 21, bg=dat$storage, ylim=c(0.40,0.80))
legend(x= "topright", pch = rep(21,2), pt.bg=1:2, legend=c("control","treatment"))
mod <- mod1
pred1 <- predict(mod, se = T, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(1, length(xx)))),type="response")
lines(xx,pred1$fit,lty=1)
lines(xx,pred1$fit-2*pred1$se.fit,lty=2)
lines(xx,pred1$fit+2*pred1$se.fit,lty=2)

pred2 <- predict(mod, se = T, newdata = data.frame(log.centrifuge = log(xx), f.storage = as.factor(rep(2, length(xx)))),type="response")
lines(xx,pred2$fit,col=2,lty=1)
lines(xx,pred2$fit-2*pred2$se.fit,lty=2,col=2)
lines(xx,pred2$fit+2*pred2$se.fit,lty=2,col=2)

# bands mostly overlaps, which confirms that log(centrifuge)
# is not significant 