#############################
# examples in DB chapter 2  #
#############################

library(dobson)

###################################
# First example with Poisson data #
###################################

# Numbers of chronic medical conditions reported 
# by samples of women living in large country towns 
# (town group) or in more rural areas (country group) 
# in New South Wales, Australia

dat1 <- chronic

summary(dat1)
means <- tapply(dat1$number,as.factor(dat1$place),mean)
variances <- tapply(dat1$number,as.factor(dat1$place),var)

# variance and mean are similar ... Poisson distribution 
# is plausible
# we therefore assume that y ~ Poi(lambda_k), where k 
# refers to the group (town or country)

# we want to test the null H0: lambda_1 = lambda_2 against
# H1: lambda_1 ≠ lambda_2

# under H0, maximum log-likelihood is
lambda_hat <- mean(dat1$number)
l0 <- sum(dpois(dat1$number,lambda_hat,log=T)) 
l0 # why do we obtain a negative value?
# "synsynlighets kvoten" - likelihood ratio

# under H1, maximum log-likelihood is
lambda_hat1 <- means[1]
lambda_hat2 <- means[2]

l1 <- sum(dpois(dat1$number[dat1$place=="town"],lambda_hat1,log=T))+
  sum(dpois(dat1$number[dat1$place=="country"],lambda_hat2,log=T)) 
l1

# l1 is expected be larger than l0 (why?)
# we'd like to know whether l1 is significantly larger than 
# l0 (to be discussed in the next lectures)

# residuals
sizes <- table(dat1$place)
plot(1:sizes[1],dat1$number[dat1$place=="town"],col=2)
points(1:sizes[2],dat1$number[dat1$place=="country"],col=4)
abline(h=means,col=c(2,4))
abline(h=lambda_hat)

# (standardized) residuals under H0
res0 <- (dat1$number - lambda_hat)/sqrt(lambda_hat)

(res0) # cfr table 2.2 of DB, #frequency table

res1_town <- (dat1$number[dat1$place=="town"]-lambda_hat1)/sqrt(lambda_hat1)
res1_country <- (dat1$number[dat1$place=="country"]-lambda_hat2)/sqrt(lambda_hat2)
res1 <- c(res1_town,res1_country)
table(res1)
# sum of squared residuals is a chi-square
chi0 <- sum(res0^2)
1-pchisq(chi0,nrow(dat1)-1) # value consistent with H0
# also chi0 is close to the mean of the chi-square (48)

chi1 <- sum(res1^2)
1-pchisq(chi1,nrow(dat1)-2)
# also chi0 is close to the mean of the chi-square (47)

# conclusion: it seems that we can't reject the null

###################################
# Second example with normal data #
###################################

# it's better to empty the environment now

dat <- birthweight

# we restructure the data in a more convenient way
boys <- dat[,1:2]
names(boys)<- c("age","weight")
girls <- dat[,3:4]
names(girls)<- c("age","weight")
dat2 <- rbind(boys,girls)
dat2 <- cbind(dat2,rep(c("boy","girl"),each=12))
names(dat2)[3] <- "gender"
plot(dat2$age,dat2$weight,col=rep(c(4,2),each=12))

# model 1: y_jk = alpha + beta x_jk (j=1,2, k=1...12)
beta <- cov(dat2$age,dat2$weight)/var(dat2$age)
alpha <- mean(dat2$weight)-beta*mean(dat2$age)
abline(alpha,beta)

# model 2: y_jk = alpha_j + beta x_jk
pos <- which(dat2$gender=="boy")
cov_boys <- cov(dat2$age[pos],dat2$weight[pos])
cov_girls <- cov(dat2$age[-pos],dat2$weight[-pos])
var_boys <- var(dat2$age[pos])
var_girls <- var(dat2$age[-pos])

beta_2 <- (cov_boys+cov_girls)/(var_boys+var_girls)
alpha_boys <- mean(dat2$weight[pos])-beta_2*mean(dat2$age[pos])
alpha_girls <- mean(dat2$weight[-pos])-beta_2*mean(dat2$age[-pos])
abline(alpha_boys,beta_2,col=4)
abline(alpha_girls,beta_2,col=2)

# predictions and residuals under model 2
pred_boys <- alpha_boys + beta_2*dat2$age[pos]
pred_girls <- alpha_girls + beta_2*dat2$age[-pos]
pred2 <- c(pred_boys,pred_girls)
res2 <- dat2$weight-pred2
ss2 <- sum(res2^2) 
plot(pred2,res2/sd(res2),col=rep(c(4,2),each=12))
plot(dat2$age,res2/sd(res2),col=rep(c(4,2),each=12))
qqnorm(res2/sd(res2),xlim=c(-2,2),ylim=c(-2,2))
qqline(res2/sd(res2))
# model 3: y_jk = alpha_j + beta_j x_jk
beta_boys <- cov_boys / var_boys
beta_girls <- cov_girls / var_girls

alpha_boys <- mean(dat2$weight[pos])-beta_boys*mean(dat2$age[pos])
alpha_girls <- mean(dat2$weight[-pos])-beta_girls*mean(dat2$age[-pos])
plot(dat2$age,dat2$weight,col=rep(c(4,2),each=12))

abline(alpha_boys,beta_boys,col=4,lty=2)
abline(alpha_girls,beta_girls,col=2,lty=2)

# predictions and residuals under model 3
pred_boys <- alpha_boys + beta_boys*dat2$age[pos]
pred_girls <- alpha_girls + beta_girls*dat2$age[-pos]
pred3 <- c(pred_boys,pred_girls)
res3 <- dat2$weight-pred3
ss3 <- sum(res3^2) 
plot(pred3,res3/sd(res3),col=rep(c(4,2),each=12))
plot(dat2$age,res3/sd(res3),col=rep(c(4,2),each=12))
qqnorm(res3/sd(res3))
qqline(res3/sd(res3))

# F for comparing the two models
F<-((ss2-ss3)/(2-1))/(ss3/(24-2*2))
1-pf(F,2-1,24-4)
# the improvement provided by model 3 is not significant

