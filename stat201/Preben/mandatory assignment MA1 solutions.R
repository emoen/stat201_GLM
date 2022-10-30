################
# solution MA1 #
################

#ex 2
# set.seed(341)
# n <- 100
# x <- runif(n,10,20)
# 
# X <- matrix(cbind(rep(1,n),x),n,2)
# beta <- c(0.1,0.3)
# 
# y <- rgamma(n,shape=2,rate = 2/(exp(beta[1]+beta[2]*x)))
#write.table(data.frame(cbind(y,x)),"gamma regression data.txt",sep="\t")

dat <- read.table("gamma regression data.txt")

plot(dat$x,dat$y)
n <- nrow(dat)
X <- matrix(cbind(rep(1,n),dat$x),n,2)
y <- dat$y
# IWLS iteration
beta <-c(0.5,0.5)
beta.seq <- array(dim=c(10,2))
for(step in 1:10) {
  
  eta<- X%*%beta  
  z <- eta + (y-exp(eta))/exp(eta)
  beta.seq[step,] <- solve(t(X)%*%X)%*%t(X)%*%z
  beta <- beta.seq[step,] 
}
beta.seq  

#standard errors
sqrt(0.5*diag(solve(t(X)%*%X)))

# plotting regression line  
xx <- seq(0,100,by=0.1)
lines(xx,exp(beta[1]+beta[2]*xx))

# check
glm(y  ~ x, data=dat, family= Gamma(link="log") )

# ex 3
#dat <- airquality
#dat$wind_speed <- cut(dat$Wind,quantile(dat$Wind))
#dat <- dat[,c(1:2,4,7)]
#write.table(dat,"pollution.txt",sep="\t")
dat <- read.table("pollution.txt")


# first model
mod <- lm(Ozone ~ Solar.R + Temp + wind_speed, data = dat)
summary(mod)

# normality check
r <- rstandard(mod)
qqnorm(r,ylim=c(-2,2))
qqline(r)
#normality reasonable

#outliers
table(cooks.distance(mod)>1)
# no outliers

# prediction and confidence interval
pred <- predict(mod,newdata = data.frame(Solar.R=100,Temp=70, wind_speed="(1.7,7.4]"),se.fit = T)
pred$fit - qnorm(0.975)*pred$se.fit
pred$fit + qnorm(0.975)*pred$se.fit
# alternatively
pred_int <- predict(mod,newdata = data.frame(Solar.R=100,Temp=70, wind_speed="(1.7,7.4]"), interval = "confidence")

# looking for significant interactions
mod_1 <- lm(Ozone ~ Solar.R + Temp + wind_speed + Temp:wind_speed, data = dat)
summary(mod_1)
anova(mod, mod_1) # barely significant

mod_2 <- lm(Ozone ~ Solar.R + Temp + wind_speed + Temp:Solar.R, data = dat)
anova(mod, mod_2) # significant

mod_3 <- lm(Ozone ~ Solar.R + Temp + wind_speed + wind_speed:Solar.R, data = dat)
anova(mod, mod_3) # not significant

# a possible new model is 
mod_2 <- lm(Ozone ~ Solar.R + Temp + wind_speed + Temp:Solar.R, data = dat)
anova(mod_2) 

# alternatively, we could use the step function
mod_full <- lm(Ozone ~ (Solar.R+Temp+wind_speed)^2, data=dat)
step(mod_full)
