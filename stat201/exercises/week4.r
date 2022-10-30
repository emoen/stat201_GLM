# 4.2: Load the dataset achievement from the library Dobson and write you own R
# script to replicate the results of Section 6.5.

library(dobson)
data( achievement )
summary( achievement )
achievement

#To test H_0: to difference in training method for A, B, C:
# E(Y_jk)= u_j + g*x_jk for H_1, Reduced model  E(Y_jk)= u + g*x_jk

#saturated model
Y= c(1,2,3)               #y_1, y_2, y_3
B= c(0.5, 0.5, 0.5, 0.5)  #u_1, u_2, u_3, g

ones  = rep(1, 7)
zeros = rep(0, 7)
x_1 =  achievement[achievement$method=='A',,][3]
x_2 =  achievement[achievement$method=='B',,][3]
x_3 =  achievement[achievement$method=='C',,][3]
x = c(c(ones, zeros, zeros, x_1),
      c(zeros, ones, zeros, x_2),
      c(zeros, zeros, ones, x_3))

t_x_x = c(7, 0, 0, 15,0, 7, 0, 24,0, 0, 7, 19,15,24,19,196)
t_x_x = matrix(data=t_x_x, nrow=4)
t_x_y = matix(c(31,53,47,398), nrow=1)
b=solve(t_x_x) %*% t_x_y

P=4
J=P-1

# 3: Sugar Australia
period = seq(1:6)
refined= c(32, 31.2, 27, 21, 14.9, 8.8)
in_food = c(16.3, 23.1, 23.6, 27.7, 34.6, 33.9)
sugar = matrix(data=c(period, refined, in_food), nrow = 6)

plot(period, in_food)
points(period, refined, pch=23)

dataf = data.frame(period=period, refined=refined, in_food=in_food)
lm_ref = lm(dataf$refined~dataf$period, data=dataf )
lm_in_food = lm(dataf$in_food~dataf$period, data=dataf)

preds = predict(lm_ref)
lines(dataf$period, preds)
summary(lm_ref)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   39.5733     1.8992   20.84 3.13e-05 ***
# dataf$period  -4.8829     0.4877  -10.01 0.000559 ***
confint(lm_ref)
#2.5 %    97.5 %
#  (Intercept)  34.300201 44.846466
#dataf$period   -6.236872 -3.528842
t.val = qt(0.975, length(period)-1) #-1.938801
-4.8829-(0.4877)*qt(0.975, length(period)-2)  #t=4
#[1] -6.236972
-4.8829+(0.4877)*qt(0.975, length(period)-2) 
#[1] -3.528828

preds = predict(lm_in_food)
lines(dataf$period, preds)
summary(lm_in_food)
#             Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   13.8733     1.9106   7.261  0.00191 **
#  dataf$period   3.6171     0.4906   7.373  0.00180 **

# 6.6 2-way anova on workers
#Perform an analysis of variance to test for differences among workers,
#among days, and possible interaction effects.
data( machine )
summary( machine )
machine = head(machine, 40)
machine[37, 3]=32.9

a = unique(machine[1]) # 2 days
b = unique(machine[2]) # worker =1,2,3,4
c = unique(machine[3]) #not unique

