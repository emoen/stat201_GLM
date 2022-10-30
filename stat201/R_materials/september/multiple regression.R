###########################
#  multiple regression    #
###########################

library(dobson)

# multiple regression , #table 6.6

data(carbohydrate) #list the available data sets.
dat <- carbohydrate
plot(dat)

mod3 <- lm(carbohydrate ~  age + weight + protein , data = dat)

summary(mod3) # the main output

# residuals
e3 <- residuals(mod3)
summary(e3)

#######################
# plotting prediction
#install.packages('ggeffects')
library(ggeffects)
#########
install.packages('ggiraph')
install.packages('ggiraphExtra')
install.packages('plyr')
require(ggiraph)
require(ggiraphExtra)
require(plyr)
plot(ggpredict(mod3, se = TRUE, interactive = TRUE, digits = 3),dat$carbohydrate)
ggplot(ggpredict(mod3, se = TRUE, interactive = TRUE, digits = 3))
points((dat$carbohydrate))
ggpredict(mod3,interactive = TRUE)
########################

# estimates
mod3 <- lm(carbohydrate ~  age + weight + protein , x=TRUE, data = dat)
X <- mod3$x
solve(t(X)%*%X)%*%t(X)%*%dat$carbohydrate #solves b: (x^t*X)^-1 * X^t * y
coef(mod3)
# standard errors
sigma <- sqrt(sum(e3^2)/(nrow(dat)-4))
sd.coef <- sigma*sqrt(diag(solve(t(X)%*%X)))
# t values
t.coef <- coef(mod3)/sd.coef
#p-values
2*(1-pt(abs(t.coef),nrow(dat)-4)) #The Student t Distribution, Deviance or 2*loglikelihood ratio - chi(p) dist

# R squared
y <- dat$carbohydrate
n <- nrow(dat)
(t(coef(mod3))%*%t(X)%*%y-n*mean(y)^2)/(sum(y^2)-n*mean(y)^2)

# F test
num <- (t(coef(mod3))%*%t(X)%*%y-n*mean(y)^2)/(4-1)
den <- (sum(y^2)-t(coef(mod3))%*%t(X)%*%y)/(n-4)
F <- num/den
1-pf(F,3,16)

# standardized residuals
e3st <- rstandard(mod3) #Regression Deletion Diagnostics
qqnorm(e3st,ylim=c(-2,2))
abline(c(0,1))
# residuals look normally distributed

# predicted values
fitted3 <- predict(mod3) # general function, provides several options 
# you can also use fitted.values, less general than predict
# however note that
table(fitted3 == fitted.values(mod3)) # fitted is a generic function which extracts fitted values from objects returned by modeling 
table(round(fitted3,5) == round(fitted.values(mod3),5))  

plot(fitted3, e3st)
abline(h=0)

# compare the following results with table 6.6
# provides several diagnostics (leverages, dfbetas, sigmas and residuals)
influence(mod3) #This function provides the basic quantities which are used in forming a wide variety of diagnostics for checking the quality of regression fits.

h <- influence(mod3)$hat
sigma <- sqrt(sum(e3^2)/(nrow(dat)-4)) # residual standard error

# check std residual definition
e3st[1]
e3[1]/(sigma*sqrt(1-h[1]))

#deleting first observation
dat1 <- dat[-1,]
mod <-lm(carbohydrate ~  age + weight + protein , data = dat1)
coef(mod3) - coef(mod)
influence(mod3)$coefficients[1,]

dfits <- e3st*sqrt(h/(1-h))
dfits
cooks.distance(mod3)
# check
(1/4)*(h/(1-h))*e3st^2



# anova decomposition
mod3 <- lm(carbohydrate ~  age + weight + protein , x=TRUE, data = dat)
anova(mod3)

# residual deviance
sum(dat$carbohydrate^2)-t(coef(mod3))%*%t(mod3$x)%*%dat$carbohydrate
# to check the other deviance components we have to estimate
# a battery of models by adding one covariate at the time
mod1 <- lm(carbohydrate ~ age, x=TRUE, data = dat)

anova(mod1)
t(coef(mod1))%*%t(mod1$x)%*%dat$carbohydrate - nrow(dat)*mean(dat$carbohydrate)^2
# residual
sum(dat$carbohydrate^2)-t(coef(mod1))%*%t(mod1$x)%*%dat$carbohydrate

mod2 <- lm(carbohydrate ~ age + weight, x=TRUE, data = dat)
anova(mod2)
t(coef(mod2))%*%t(mod2$x)%*%dat$carbohydrate-
  t(coef(mod1))%*%t(mod1$x)%*%dat$carbohydrate
# residual
sum(dat$carbohydrate^2)-t(coef(mod2))%*%t(mod2$x)%*%dat$carbohydrate


anova(mod3)
t(coef(mod3))%*%t(mod3$x)%*%dat$carbohydrate-
  t(coef(mod2))%*%t(mod2$x)%*%dat$carbohydrate

# residual
sum(dat$carbohydrate^2)-t(coef(mod3))%*%t(mod3$x)%*%dat$carbohydrate


# DB example: we want to test the impact of age
# model without age
mod_noage <- lm(carbohydrate ~ weight + protein, x=TRUE, data = dat)
anova(mod_noage,mod3)

sum(dat$carbohydrate^2)-t(coef(mod_noage))%*%t(mod_noage$x)%*%dat$carbohydrate
sum(dat$carbohydrate^2)-t(coef(mod3))%*%t(mod3$x)%*%dat$carbohydrate
F <- ((606.02 -567.66)/1)/(567.66/16)
1-pf(F,1,16)

# conclusion: age does not significantly reduce residual deviance

