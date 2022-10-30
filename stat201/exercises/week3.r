#2. Load the leukemia dataset from the library Dobson
# It includes times to death, yi, in weeks from diagnosis and 
# log10 (initial white blood cell count), xi, for leukemia patients. 
#Plot the data to have a rough idea of the relationship between the two varables. 
# Assume that y_i ~ theta_i * exp(-theta_i * y_i) - exponential distribution
# E[y_i] = exp(B_0 + B1*x_i)

# a) Plot, #Is this a GLM? Motivate your answer
library(dobson)

data( leukemia )
summary( leukemia )
leukemia

#model 1
plot(leukemia$wbc, leukemia$time)

mod = lm(leukemia$time ~ leukemia$wbc , data = leukemia)
preds = predict(mod)
lines(leukemia$wbc, preds)


summary(mod)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    306.21      67.69   4.524 0.000404 ***
#  leukemia$wbc   -59.51      16.35  -3.640 0.002418 **
# log(y_i) = 306 -59.5*x_i
plot(leukemia$wbc, leukemia$time)
abline(306,-59.5)

#model 2
plot(leukemia$wbc, leukemia$time)

mod = lm(leukemia$time ~ leukemia$wbc , data = leukemia)
preds = predict(mod)
lines(leukemia$wbc, preds)

exponential.model = glm(leukemia$time ~ leukemia$wbc, family=Gamma(link="identity"), data=leukemia ) #family="exponential")#(link="log"))

#to get 95% CI
confint(exponential.model)
plot(leukemia$wbc, leukemia$time)
preds = predict(exponential.model)
lines(leukemia$wbc, preds)

#dispersion=1 for exponential distribution
summary(exponential.model, dispersion=1)

##############################
##### log-transform y - time #
##############################
exponential.model2 = glm(leukemia$time ~ leukemia$wbc, family=Gamma(link="log"), data=leukemia )
preds = predict(exponential.model2)
plot(leukemia$wbc, log(leukemia$time))
lines(leukemia$wbc, preds)
abline(8.47, -1.1)

#glm(dat ~ x, family = poisson(link = "log") 
#glm(dat ~ x, family = poisson(link = "sqrt")  
#glm(dat ~ x, family = poisson(link = "identity"))

################################
# b) does data show trend
# b) calc score matrix U(B), information matrix I(B)

scale(leukemia) #score vector U(B)
#attr(,"scaled:center")
#time       wbc 
#62.470588  4.095882 
#attr(,"scaled:scale")
#time        wbc 
#54.3531481  0.6255404 

#Fisher information
install.packages("maxLik")
library(maxLik)
vcov(mod)
fisher_information = vcov(mod)^-1 #information matrix I(B)

#####################
# c) Write an R code that uses such iteration and obtain the MLE of β

# c) A possible specification for E(Y ) is
# E(Yi) = exp(1 + 2xi),
# which will ensure that E(Y ) is non-negative for all values of the parameters and all values of x. 
# Which link function is appropriate in this case?

# 95% CI of B_1
mod = lm(leukemia$time ~ leukemia$wbc , data = leukemia)
summary(mod)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)    306.21      67.69   4.524 0.000404 ***
#  leukemia$wbc   -59.51      16.35  -3.640 0.002418 ** 
exp_mod=glm(leukemia$time~leukemia$wbc, family=Gamma(link="log"))
summary(exp_mod, dispersion=1)
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    8.4775     1.6548   5.123 3.01e-07 ***
#  leukemia$wbc  -1.1093     0.3997  -2.776  0.00551 ** 
plot(leukemia$wbc, leukemia$time)
preds = predict(mod)
lines(leukemia$wbc, preds) #lm 
preds = predict(exp_mod)
lines(leukemia$wbc, preds) #glm
abline(8.47, -1.1)

#I've tried to solve the Es. 12, cap 4 of "Introduction to GLM" by Annette Dobson.
#It's about the relationship between survival time of leukemia patients and blood cell count.
#I tried to fit a model with exponential distribution, first by glm (family gamma and then dispersion parameter fixed to 1) and 
#then with survreg.
#They gave me the same point estimates but the standard errors are slightly different.
#I checked the results building the routine manually, and it seems that the glm results are the correct one.
 
#######################################################
data <- data.frame(y=c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65),
 x=c(3.36,2.88,3.63,3.41,3.78,4.02,4.00,4.23,3.73,3.85,3.97,4.51,
 4.54,5.00,5.00,4.72,5.00))

model1 <- glm(y~x,family=Gamma(link="log"),data)
summary(model1,dispersion=1)
model2 <- survreg(Surv(y) ~ x, data, dist="exponential")
summary(model2)

X <- model.matrix(model1)
y <- as.vector(data$y)
b <- as.matrix(c(1,1))  # STARTING VALUES
iter <- matrix(0,7,2)
iter[1,] <- b
for (i in 2:7) {
 W <- diag(rep(1,length(y)))
 z <- X%*%b + (y-exp(X%*%b))*1/exp(X%*%b)
 b <- solve(t(X)%*%W%*%X) %*% (t(X)%*%W%*%z)
 iter[i,] <- b
}
summary(model1,dispersion=1)$coef
summary(model2)
iter[nrow(temp),]
sqrt(diag(solve(t(X)%*%W%*%X)))
#######################################################

#Can you explain if this difference is due to an error?
#Thanks in advance

##################################
##### Wald-test ##################
##################################
x = leukemia$wbc
intercept = rep(1, length(x))
x = cbind(intercept,x) 

y = leukemia$time

b = solve( (t(x) %*% x) ) %*% t(x) %*% y #MLE estimator of p= (306, -59)

#exponential.model = glm(leukemia$time ~ leukemia$wbc, family=Gamma(link="identity"), data=leukemia )
#summary(exponential.model, dispersion=1)
# 248.75, -46.10

#degrees of freedom df = n-k = 17-2
df = length(x) - length(x[1,])
#residuals s^2
coeff_b0 = beta.seq[10,1]
coeff_b1 = beta.seq[10,2]
preds = coeff_b0 + coeff_b1*x[,2]
residuals = abs(preds-y)
s2 = sum(residuals^2)/df

#information
V_b = s2 * solve( (t(x) %*% x) )

std_err_b0 = sqrt(V_b[1,1])
std_err_b1 = sqrt(V_b[2,2])

#resid(exponential.model) will default to the deviance residuals
#exponential.model$resid will give you the working residual
s2 = sum( exponential.model$resid^2 )/df
#model residuals and fisher information
V_b = s2 * solve( (t(x) %*% x) )

std_err_b0 = sqrt(V_b[1,1])
std_err_b1 = sqrt(V_b[2,2])

# t-statistics
t_stat_b0 = coeff_b0 / std_err_b0
t_stat_b1 = coeff_b1 / std_err_b1

#test if coefficients different to 0
two_tailed_p_b0 = 2*pt(q=t_stat_b0, df=df, lower.tail = FALSE) # p=0.85
two_tailed_p_b1 = 2*pt(q=abs(t_stat_b1), df=df, lower.tail = FALSE) # p=0.92


################ CONFINT2 ########################
#confint2: Confidence Intervals for Generalized Linear Models
##################################################
#Computes confidence intervals based on Wald, likelihood-ratio, Rao's score or Terrell's gradient tests for a generalized linear model.
install.packages("glmtoolbox")
library(glmtoolbox)
help(glmtoolbox)
confint2(exponential.model, test="lr")
confint2(exponential.model, test="score")
confint2(exponential.model, test="wald")

summary(exponential.model) #249-46x, (95.14, 19.9)
confint2(exponential.model, test="wald") #(69, 428)b_0, (-83, -8.5)b_1
################################################################################
exp_mod=glm(leukemia$time~leukemia$wbc, family=Gamma(link="log"))
summary(exp_mod, dispersion=1) # 8.477  -1.109
confint2(exp_mod, test="wald") #(5.33, 11.62) (-1.8683,-0.35)

s2 = sum( exp_mod$resid^2 )/df
#model residuals and fisher information
V_b = s2 * solve( (t(x) %*% x) )
std_err_b0 = sqrt(V_b[1,1])
std_err_b1 = sqrt(V_b[2,2])
################################################################################

##############################
### fisher scoring using IWLS
##############################
n = length(leukemia$wbc)
X = matrix(cbind(rep(1, n), c(leukemia$wbc)), nrow=n, ncol=2)
y = leukemia$time
W = diag(1, nrow=n, ncol=n)
# IWLS iteration
beta <-c(0.5,0.5)
beta.seq <- array(dim=c(10,2))
for(step in 1:10) {
  
  eta<- X%*%beta  
  z <- eta + (y-exp(eta))/exp(eta)
  beta.seq[step,] <- solve(t(X)%*%X)%*%t(X)%*%z
  beta <- beta.seq[step,] 
}
beta.seq  #8.491880 -1.112696
plot(leukemia$wbc, log(leukemia$time))
abline(exp(8.49188), exp(-1.11))