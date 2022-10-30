##################
# ANOVA models   #
##################

library(dobson)

# one-way anova

data(plant.dried)
dat <- plant.dried
summary(dat)
# a factor with 3 levels (balanced design)
table(dat$group)

# we begin with a linear model
mod <-lm(weight ~ group, x=TRUE, data = dat)
summary(mod)

# the function anova creates the ANOVA table
anova(mod)

#How to obtain the table components

# Sum of Squares Regression
ssr <- sum((fitted(mod) - mean(dat$weight))^2)
ssr
#or, alternatively, using matrix form (see lecture 12):
t(coef(mod))%*%t(mod$x)%*%dat$weight - nrow(dat)%*%mean(dat$weight)^2

# Sum of Squares Error
sse <- sum((fitted(mod) - dat$weight)^2)
sse
#or, alternatively, using matrix form (see lecture 12):
sum(dat$weight^2)-t(coef(mod))%*%t(mod$x)%*%dat$weight







# two-way anova

data(balanced)
dat <- balanced
names(dat) <- c("A","B","y")
str(dat)

# a model with interaction
mod <-lm(y ~ A+B+A:B, x=TRUE, data = dat)
mod <- lm(y ~ A*B, x=TRUE, data = dat)
summary(mod)
tapply(dat$y,list(dat$A,dat$B),mean)
# this model is saturated: 6 means are replaced 
# by six parameters: can you guess the relationship between
# means and parameters? (easy: recall lecture 12)
mod$coefficients

# a model without interaction
mod_noint <-lm(y ~ A+B, x=TRUE, data = dat)
summary(mod_noint)
tapply(dat$y,list(dat$A,dat$B),mean)
mod_noint$coefficients
# what is the relationship between means and parameters?
# a bit more difficult than before
# Since there is no interaction term, we need to take into consideration
# both of the levels in the B column!
((7.45-6.70)+(6.85-5.70))/2 
((8.45-6.70)+(8.95-5.70))/2
# we need to consider the differences between all the rows of the mean table!
((5.70-6.70)+(6.85-7.45)+(8.95-8.45))/3
# Here it becomes a bit complicated...
# only column interaction
First_row<-6.7+5.7+0.3667 
# column interaction plus interaction between 1st and 2nd row!
Second_row<-7.45-0.95 + 6.85-0.95+0.3667 
# column interaction plus interaction between 1st and 3rd row!
Third_row<-8.45-2.5+8.95-2.5+0.3667
# We divide by the total number of means, which is 6
(First_row+Second_row+Third_row)/6



