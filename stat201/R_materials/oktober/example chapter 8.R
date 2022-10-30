# multinominal logistic regression
# example DB 8.3.1

library(dobson)
library(nnet) # package to estimate multinomial logistic regression 
data(Cars)
dat <- Cars
?Cars

# preliminaries: this is important for
# setting reference categories
summary(dat) # categorical variables treated as strings
dat$sex <- factor(dat$sex)
dat$age <- factor(dat$age)
dat$response <- factor(dat$response)
summary(dat) # categorical variables treated now as factors
# but with levels in alphabetical order
dat$sex <- factor(dat$sex, levels = c("women","men"))
dat$age <- factor(dat$age, levels = c(  "18-23","24-40","> 40" ))
dat$response <- factor(dat$response, levels = c(  "no/little","important","very important" ))
summary(dat)# now something to work with!

# plotting the data
# observed proportions
totals <- c(t(with(dat, tapply(frequency, list(sex, age), sum))))
totals
obs <-  dat$frequency/rep(totals,each=3)
obs.women <- obs[1:9]
obs.men <- obs[10:18]
#women
plot(rep(1:3,each=3),obs.women,ylim=c(0,1),pch=22,bg=c(1:2,4),
     xaxt="n",xlab="age",ylab="obs. proportions") # no/little important
axis(1,at=1:3,labels=levels(dat$age))
legend("topleft",pch=rep(21,3),pt.bg=c(1:2,4),
       legend=levels(dat$response))
title(main="women")
#men
plot(rep(1:3,each=3),obs.men,ylim=c(0,1),pch=22,bg=c(1:2,4),
     xaxt="n",xlab="age",ylab="obs. proportions") # no/little important
axis(1,at=1:3,labels=levels(dat$age))
legend("topleft",pch=rep(21,3),pt.bg=c(1:2,4),
       legend=levels(dat$response))
title(main="men")


# multinomial logistic model
mod <- multinom(response ~ sex + age, weights = frequency, data = dat)

summary(mod)      # watch: residual deviance is -2loglik 

# recall that AIC = -2logl+2p 
# then logl =(2p - AIC)/2
logl <- (2*8-mod$AIC)/2
logl
-2*logl
# fit the null model and compute the likelihood ratio statistic

mod0 <- multinom(response ~ 1, weights = frequency, data = dat)
logl0 <- (2*2-mod0$AIC)/2

chi <- 2*(logl-logl0)
1-pchisq(chi,8-2)
# indicates that the included covariates are significant

# fit the saturated model
mod_full <- multinom(response ~ sex + age + sex:age, weights = frequency, data = dat)
summary(mod_full)      
logl_full <- (2*12-mod_full$AIC)/2

# residual deviance
D <- 2*(logl_full-logl) 
1-pchisq(D, 12-8)
# no significant gain by adding interactions   

# predicted probabilities
pred <- predict(mod,type = "probs"  )
pred
pred.women <- pred[1:9,]
pred.men <- pred[10:18,]
# women:
plot(rep(1:3,each=3),pred.women[,1],ylim=c(0,1),pch=21,bg=1,
     xaxt="n",xlab="age",ylab="probabilities") # no/little important
points(rep(1:3,each=3),obs.women,pch=22,bg=c(1:2,4))
lines(rep(1:3,each=3),pred.women[,1],col=1)
points(rep(1:3,each=3),pred.women[,2],pch=21,bg=2)# important
lines(rep(1:3,each=3),pred.women[,2],col=2)
points(rep(1:3,each=3),pred.women[,3],pch=21,bg=4)# very important
lines(rep(1:3,each=3),pred.women[,3],col=4)
axis(1,at=1:3,labels=levels(dat$age))
legend("topleft",pch=rep(21,3),pt.bg=c(1:2,4),
       legend=levels(dat$response))
title(main="women")
#men:
plot(rep(1:3,each=3),pred.men[,1],ylim=c(0,1),pch=21,bg=1,
     xaxt="n",xlab="age",ylab="probabilities") # no/little important
points(rep(1:3,each=3),obs.men,pch=22,bg=c(1:2,4))
lines(rep(1:3,each=3),pred.men[,1],col=1)
points(rep(1:3,each=3),pred.men[,2],pch=21,bg=2)# important
lines(rep(1:3,each=3),pred.men[,2],col=2)
points(rep(1:3,each=3),pred.men[,3],pch=21,bg=4)# very important
lines(rep(1:3,each=3),pred.men[,3],col=4)
axis(1,at=1:3,labels=levels(dat$age))
legend("topleft",pch=rep(21,3),pt.bg=c(1:2,4),legend=levels(dat$response))
title(main="men")


