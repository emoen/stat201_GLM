##########################
# linear models with R   #
##########################

#install.packages("faraway")
library(faraway)


#########
# LAB 1 #
#########

# EX 1
data(teengamb)
?teengamb

# Fit a regression model with the expenditure on 
# gambling as the response and the sex, status, 
# income and verbal score as predictors.



# a. What percentage of variation in the response is explained by these predictors?



# b. Which observation has the largest (positive) residual?


# c. For all other predictors held constant, 
# what would be the difference in predicted
# expenditure on gambling for a male compared to a female?


# EX 2
 
### inport dataset "cholesterol.txt"

# The cholesterol dataset include cholesterol 
# reduction (negative values mean cholesterol increase)
# in subjects who have been treated by a medication or not
# included variables are treated (1=treated, 0 = untreated ),
# age (in years) and cholesterol reduction

# a. Load the data and fit the anova model
# reduction = beta0 + beta1 treated
# interpret the results

# b. Now fit the ancova model
# reduction = beta0 + beta1 treated + beta2 age
# compare with the results above: are the two models
# consistent?



# EX 3
# An example on how to select the best fitting
# model using an AIC-based backward selection method
data(state)
statedata <- data.frame(state.x77,row.names=state.abb,check.names=T)
# We fit a model with all the variables as explanatory variables
mod <- lm(Life.Exp ~ ., data=statedata)
summary(mod)
# We use the function "step" to find the lowest AIC value
# (we will discuss this more in class) 
step(mod)
# Check the linear model assumptions via appropriate
# diagnostics
fit <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, 
          data = statedata)
plot(fit$fitted.values,fit$residuals)
abline(h=0)
# variance seems constant

# qq residual plots
qqnorm(residuals(fit),ylab="Raw Residuals")
qqline(residuals(fit))
# studentized residuals
qqnorm(rstudent(fit),ylab="Studentized Residuals")
abline(0,1)

# data seem normally distributed


# EX 2
# Dataset twins
data(twins)
# comes from a 1966 paper by Cyril Burt entitled 
# "The genetic determination of differences in 
# intelligence: A study of monozygotic twins reared
# together and apart". The data consist of IQ scores for identical
# twins, one raised by foster parents, the other 
# by the natural parents. We also know the social 
# class of natural parents (high, middle or low).
# Fit a full model by using the foster IQ, 
# starting with a model that includes interactions,
# and propose a final model.

