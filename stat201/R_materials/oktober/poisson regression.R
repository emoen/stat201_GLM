######################
# Poisson regression #
######################

library(dobson)
library(tidyr) # to spit strings

data(doctors)
dat <- doctors

# creating a numerical variable "age"
df <- data.frame(x = dat$age )
name_col <- df %>% separate(x, c("A", "B","C"))
dat$Age <- (as.numeric(name_col$A)+as.numeric(name_col$C))/2

# note the definition of variable person-years (symbol "-" is problematic)
plot(dat$Age,dat$deaths/dat$'person-years',pch=21, bg=rep(1:2,each=5)) 
# as in the book (typically made in epidemiology)
plot(dat$Age,100000*dat$deaths/dat$'person-years',pch=21, bg=rep(1:2,each=5)) 

# model proposed by DB:
dat$Age.DB <- as.factor(dat$Age)
levels(dat$Age.DB) <- 1:5
dat$Age.DB <- as.numeric(dat$Age.DB)

mod <- glm(deaths ~ offset(log(dat$'person-years')) + smoking 
           + Age.DB + I(Age.DB^2) + Age.DB:smoking, 
           data = dat, family = poisson())
summary(mod)

# goodness of fit
plot(dat$Age.DB,dat$deaths,col = rep(1:2,each=5))
plot(dat$Age.DB,dat$deaths/dat$'person-years',col = rep(1:2,each=5))
pred <- predict(mod,type="response")
lines(1:5,pred[1:5]/dat$'person-years'[1:5])
lines(1:5,pred[6:10]/dat$'person-years'[6:10],col=2)
# looks very good; let's check the residuals
plot(residuals(mod))
abline(h=0)
# they look good; let's compute the deviance 
# components
D_res <- mod$deviance
1-pchisq(D_res,nrow(dat)-5) # good fit
D_mod <- mod$null.deviance-D_res
1-pchisq(D_mod,5-1) # we reject the null model


# some better definitions
dat$Age.base <- dat$Age-min(dat$Age)
mod <- glm(deaths ~ offset(log(dat$'person-years')) + smoking 
           + Age.base + I(Age.base^2) + Age.base:smoking, 
           data = dat, family = poisson())

summary(mod)
# model is equivalent, only parameter interpretation is 
# different


################################ 
# case study: cancer mortality #
################################

# number of deaths and exposed to risk by 
# gender, age and province
# in Italy (1999)
dat <- read.table("cancer.deaths.txt")
summary(dat)						

which(dat$E==0) 
dat <- dat[-7777,] # exclude this profile (structural zero)
summary(dat)

plot(dat$n,cex=0.8,pch=21,bg=1,xlab="")
plot(dat$n/dat$E,cex=0.8,pch=21,bg=1,xlab="")

# estimating the average death rate
plot(dat$n/dat$E,cex=0.8,pch=21,bg=1,xlab="")
abline(h=sum(dat$n)/sum(dat$E),col=2,lwd=2)

plot(log(dat$n/dat$E),cex=0.8,pch=21,bg=1,xlab="")
abline(h=log(sum(dat$n)/sum(dat$E)),col=2,lwd=2)

# estimating the average death rate as the intercept
# of a Poisson regression without covariates
mod.0 <- glm(n ~ offset(log(E)), family= poisson(),data=dat)
coef(mod.0)
log(sum(dat$n)/sum(dat$E))

plot(log(dat$n/dat$E))
abline(h=coef(mod.0),lwd=2,col=2)

# including age
plot(dat$age,log(dat$n/dat$E),cex=0.8,pch=21,bg=1)
mod.1 <- glm(n ~ offset(log(E))+age, family= poisson(),data=dat)
coef(mod.1)
abline(mod.1,lwd=2,col=2)

# the Gompertz curve
plot(dat$age,dat$n/dat$E,cex=0.8,pch=21,bg=1)
points(dat$age, mod.1$fitted.values/dat$E,pch=21,bg=2)
title(main="Gompertz curve")


# including gender
pos.male <- which(dat$sex=="M")
pos.female <- which(dat$sex=="F")
plot(dat$age[pos.male],log(dat$n/dat$E)[pos.male],cex=0.8,pch=21,bg="blue",xlab="")
points(dat$age[pos.female],log(dat$n/dat$E)[pos.female],cex=0.8,pch=21,bg="magenta",xlab="")

mod.2 <- glm(n ~ offset(log(E))+age+sex, family="poisson",data=dat)
summary(mod.2)
coef(mod.2)
abline(coef(mod.2)[1:2],lwd=2,col="magenta")
coefM.intercetta <- sum(coef(mod.2)[c(1:3)])
abline(coefM.intercetta,coef(mod.2)[2],lwd=2,col="blue")

# subsetting the data
dat30 <- subset(dat,dat$age>30)
dat30$age <- dat30$age-30
plot(dat30$age[pos.male],log(dat30$n/dat30$E)[pos.male],cex=0.8,pch=21,bg="blue",xlab="")
points(dat30$age[pos.female],log(dat30$n/dat30$E)[pos.female],cex=0.8,pch=21,bg="magenta",xlab="")

mod.30 <- glm(n ~ offset(log(E))+age+sex, family="poisson",data=dat30)
summary(mod.30)
coef(mod.30)
abline(coef(mod.30)[1:2],lwd=2,col="magenta")
coefM.intercetta <- sum(coef(mod.30)[c(1:3)])
abline(coefM.intercetta,coef(mod.30)[2],lwd=2,col="blue")

# significant interaction?
mod.30.int <- glm(n ~ offset(log(E))+age+sex+age:sex, family="poisson",data=dat30)
summary(mod.30.int)

step(mod.30.int)
# goodness of fit
plot(residuals(mod.30.int))
abline(h=0,col=2,lwd=2)
qqnorm(residuals(mod.30.int))
qqline(residuals(mod.30.int))
# they look not very normal; let's compute the deviance 
# components
D_res <- mod.30.int$deviance
1-pchisq(D_res,nrow(dat30)-4) # there are many large residuals
D_mod <- mod.30.int$null.deviance-D_res
1-pchisq(D_mod,4-1) # we reject the null model: included effects are relevant
