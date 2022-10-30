#################
# MA2 SOLUTIONS #
#################

# EXERCISE 1
dat <- read.table("mmse.txt", header=T)

# item a)
dat$adl <- as.factor(dat$adl)
mod <- glm(cbind(mmse,23-mmse)~ age + gender + urbrur +
             act + adl , data = dat, family="binomial")
summary(mod)
# intepretation: the probabiity of a correct answer significantly 
# decreases with age and adl limits; females perform worse than 
# males (it is possible that Chinese females
# are less educated than males); active subjects with urban residence and active life style 
# perform better than sedentary subjects with a rural 
# residence and sedentary lifestyle

# item b) 
x <- seq(80,100, length=100)*12
p <- predict(mod, newdata = data.frame(age = x, gender=0, 
            urbrur = 0, act = 0, 
            adl = "0"), type="response") 

plot(x,p,ylim=c(0,1))

# item c) 
mod1 <- glm(cbind(mmse,23-mmse)~ age + gender + urbrur +
             act + adl + age:gender, data = dat, family="binomial")
anova(mod,mod1)
1-pchisq(8.2983,1)
# interaction between age and gender is significant: 
# age has a different infuence between males and females
summary(mod1)
# females' performance decreases more slowly than males


# EXERCISE 2

#item a)
dat <- read.table("penalty.txt", header=T)


mod_ind <- glm(count ~ victim + defendant + penalty, family = poisson() , data = dat)
summary(mod_ind)

mod_ind$deviance
1-pchisq(mod_ind$deviance,4) 
# we reject the independence assumption

# item b)

mod_VD <- glm(count ~ victim * defendant + penalty, family = poisson() , data = dat)
mod_VP <- glm(count ~ victim * penalty + defendant , family = poisson() , data = dat)
mod_DP <- glm(count ~ victim + defendant * penalty, family = poisson() , data = dat)

mod_VD$deviance
mod_VP$deviance
mod_DP$deviance

# model with VD interaction is the best: there is a singificant
# interaction between the race of victim ad that of the defendant
# by looking back to the data, death penalty occur mostly when
# the defendant is black and the victim is white

# item c)
mod_VD_VP <- glm(count ~ victim + defendant + penalty
              + victim : defendant + victim: penalty, family = poisson() , data = dat)
mod_VD_DP <- glm(count ~ victim + defendant + penalty
           + victim : defendant + defendant: penalty, family = poisson() , data = dat)
mod_VP_DP <- glm(count ~ victim + defendant + penalty
                 + victim : penalty + defendant: penalty, family = poisson() , data = dat)

mod_VD_VP$deviance
mod_VD_DP$deviance
mod_VP_DP$deviance

summary(mod_VD_DP)
# this seems the best model

# EXERCISE 3
dat <- read.table("mmse.txt", header=T)

library(flexsurv)

# item a)
dat1 <- subset(dat, gender==0&urbrur == 0&act==0& adl=="0")
KM <- survfit(Surv(duration,status) ~ 1, data=dat1)

plot(KM)
summary(KM)

# item b)
print(KM)


# item c) 
dat$adl <- as.factor(dat$adl)
mod_gompertz <- flexsurvreg(Surv(duration,status) ~ gender + urbrur +
                              act + adl +mmse , data = dat,dist = "gompertz")
plot(mod_gompertz)
mod_gompertz

# item d) 
mod_weibull <- survreg(Surv(duration,status) ~ gender + urbrur +
                              act + adl +mmse , data = dat,dist = "weibull")
summary(mod_weibull)

# item e)
AF = exp(-coef(mod_weibull)%*%c(0,0,0,0,0,0,20))
AF
