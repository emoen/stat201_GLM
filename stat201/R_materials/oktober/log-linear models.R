##################################
# analysis of contingency tables #
##################################

library(dobson)

###########################
# a cross-sectional study #
###########################

data(melanoma)
dat <- melanoma
dat

# null model: mu = exp(alpha_0)
mod0 <- glm(frequency ~ 1, family = poisson() , data = dat)
summary(mod0)
# what is the intercept estimate?
exp(coef(mod0))
sum(dat$frequency)/12

# independence model
# let's first re-level variable site (to reproduce book's results)
ll <- levels(as.factor(dat$site)) 
dat$site <- factor(dat$site, levels = ll[c(2,1,3)])

mod_ind <- glm(frequency ~ type + site, family = poisson() , data = dat)
summary(mod_ind)

# how were these estimated computed?
# contingency table
f_tab <- as.table(matrix(dat$frequency,3,4))
f_tab
# marginals
row.m <- apply(f_tab,1,sum)/sum(f_tab)
row.c <- apply(f_tab,2,sum)/sum(f_tab)
# predicted probabilities
y_hat <- outer(row.m,row.c)*sum(dat$frequency)
y_hat
matrix(predict(mod_ind, type="response"),3,4)
# predicted probabilities are estimated by multiplying
# the MLE of the margins 
coef(mod_ind)
y_hat[1,1]
exp(coef(mod_ind)[1])
y_hat[1,2]
exp(coef(mod_ind)[1]+coef(mod_ind)[4])
# and so on ...

mod_ind$deviance
1-pchisq(mod_ind$deviance, 2*3) # we reject the model

# equivalently
r2 <- sum(residuals(mod_ind, type = "pearson")^2)
1-pchisq(r2, 2*3)
# note that that the sum of squared pearson residuals 
# is equal to the popular chi-square test statistic for
# independence in a contigency table
summary(f_tab)

# saturated model
mod <- glm(frequency ~ type * site, family = poisson() , data = dat)
summary(mod)

####################
# randomized trial #
####################

data(vaccine)
dat <- vaccine
f_tab <- as.table(t(matrix(dat$frequency,c(3,2))))

mod <- glm(frequency ~ 1, family = poisson(), data =dat) 
summary(mod)
# doesn't make much sense: it assumes that all the subjects
# are uniformly distributed across the cells, which is 
# simply not true by design
exp(2.499)*3
apply(f_tab,1,sum)

# relevel response variable
ll <- levels(as.factor(dat$response))
dat$response <- factor(dat$response, levels = ll[3:1])
# minimal model is:
mod_min <- glm(frequency ~ treatment, family = poisson(), data =dat)
# assumes that conditional distributions are uniform
summary(mod_min)
# it is consistent with the fixed row totals
exp(2.53897)*3
exp(2.53897-0.08224)*3
apply(f_tab,1,sum)
# but does not help to test possible vaccine/response association 

# independence model
mod_ind <- glm(frequency ~ treatment + response, family = poisson(), data =dat)
# assumes that conditional distributions are equal although not necessarly uniform
# which means independence between treatment and response
summary(mod_ind)
mod_ind$deviance
1-pchisq(mod_ind$deviance,2)
# the independence model doesn't fit the data well
summary(f_tab)

f_tab
p_tab <- t(matrix(predict(mod_ind , type="response"), 3,2))
(f_tab - p_tab)/sqrt(p_tab)
# matrix of pearson residuals
t(matrix(residuals(mod_ind , type="pearson"), 3,2))
# largest residuals are associated with the small response


#########################################
# a more complex setting: case-control  #
#########################################

data(ulcer)
dat <- ulcer
dat$case_control <- dat$'case-control'

# minimal model
mod1 <- glm(frequency ~ case_control * ulcer, family = poisson(), data = dat)
summary(mod1)

# adding aspirin use
mod2 <- glm(frequency ~ case_control * ulcer + aspirin, family = poisson(), data = dat)
summary(mod2)

# adding interaction between aspirin use and case/control status

mod3 <- glm(frequency ~ case_control * ulcer + aspirin:case_control, family = poisson(), data = dat)
summary(mod3)

anova(mod2,mod3)
1-pchisq(11.251, 1)
# mod3 is significantly better than model 2
# it means: aspirin use has an influence on ulcer occurrence
# looking at the point estimates of mod3 we see that
# aspirin use is a risk factor 

# adding interaction with ulcer site
mod4 <- glm(frequency ~ case_control * ulcer + aspirin:case_control
            + aspirin:ulcer, family = poisson(), data = dat)
summary(mod4)

anova(mod3,mod4)
1-pchisq(4.2555,1)
# weak evidence of significant differences between ulcer sites

# goodness of fit of model 4
1-pchisq(mod4$deviance,8-1)
# good but we'd have probably expected a better result, 
# given that this is an almost saturated model