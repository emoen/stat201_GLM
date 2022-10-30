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

# J = 2  days, # K = 4 workers, JK = 8 groups
# H_I there are no interactive effects, that is the effect of workers and days are additive
# H_W there are no difference in quality associated with workers
# H_D there are no difference in quality associated with days
# Consider 1 saturated model, and 3 reduced models
#
# Saturated model E(Y_wdl) = b0 + b1_w + b2_d + b3_wd 
# Additive model  E(Y_wdl) = b0 + b1_w + b2_d         #compare with saturated for H_i
# No worker effect E(y_wdl) = b0       + b2_d         #compare with additive for H_W
# No day effect    E(Y_wdl) = b0 + b1_w               # compare with additive for H_d
#
# calculate: 1) sse day 2) sse worker 3) sse within (error) 4) sse both factors
# sse_total = sse day + sse worker + sse within + sse both factors
# DF Satured: Additive: worker effect: day effect:

total_avg = mean(machine[,3])

#mean workers
w1 = mean(machine[machine$worker==1,][,3])
w2 = mean(machine[machine$worker==2,][,3])
w3 = mean(machine[machine$worker==3,][,3])
w4 = mean(machine[machine$worker==4,][,3])

#mean days
d1 = mean(machine[machine$day==1,][,3])
d2 = mean(machine[machine$day==2,][,3])


#1) sse day - factor 1. 2 rows => df = 2-1 = 1
sse_day1 = sum( 20*(d1-total_avg)^2 )
sse_day2 = sum( 20*(d2-total_avg)^2 )
sse_1_factor = sse_day1+sse_day2

######### 2. factor cmp to grand mean, 4 rows => df = 4 - 1 = 3
sse_w1    = sum(5*(w1-total_avg)^2 )
sse_w2    = sum(5*(w2-total_avg)^2 )
sse_w3    = sum(5*(w3-total_avg)^2 )
sse_w4    = sum(5*(w4-total_avg)^2 )
sse_2_factor = sse_w1+sse_w2+sse_w3+sse_w4

######## within error, rows = 4 => df= 4-1 = 3 * 4 * 2 = 24
within_d1_w1 = sum((machine[machine$worker==1 & machine$day==1,][,3]-w1)^2)
within_d1_w2 = sum((machine[machine$worker==2 & machine$day==1,][,3]-w2)^2)
within_d1_w3 = sum((machine[machine$worker==3 & machine$day==1,][,3]-w3)^2)
within_d1_w4 = sum((machine[machine$worker==4 & machine$day==1,][,3]-w4)^2)
sse_within_d1 = within_d1_w1 + within_d1_w2 + within_d1_w3 + within_d1_w4
  
within_d2_w1 = sum((machine[machine$worker==1 & machine$day==2,][,3]-w1)^2)
within_d2_w2 = sum((machine[machine$worker==2 & machine$day==2,][,3]-w2)^2)
within_d2_w3 = sum((machine[machine$worker==3 & machine$day==2,][,3]-w3)^2)
within_d2_w4 = sum((machine[machine$worker==4 & machine$day==2,][,3]-w4)^2)
sse_within_d2 = within_d2_w1 + within_d2_w2 + within_d2_w3 + within_d2_w4
sse_within= sse_within_d1 + sse_within_d2

####### sse total, df = sum() = 1+3+24+3= 31
sse_t = sum( (machine[,3]-total_avg)^2 )

aggr_sse_t =sse_1_factor + sse_2_factor + sse_within

####### sse both factors , df = df_1_factor * df_2_factor = 1*3 = 3
sse_both = sse_t - aggr_sse_t


########### mean square
mse_1_factor = sse_1_factor/1
mse_within = sse_within/24

f_1 = sse_1_factor / mse_within # F(1,24)

mse_2_factor = sse_2_factor/3
f_2 = mse_2_factor / mse_within # F(3,24) # h_0 false

mse_both_factors = sse_both/3
f_3 = mse_both_factors / mse_within # F(3, 24) #h_0 false

#find F critical value
qf(p=.05, df1=3, df2=24, lower.tail=FALSE)


##############################
# 2-way anova in R with aov
##############################
twoway  = data.frame(machine)
twoway$day <- factor(machine$day, levels = c(1, 2), labels = c("D1", "D2"))
twoway$worker <- factor(machine$worker, levels = c(1, 2, 3, 4), labels = c("w1", "w2", "w3", "w4"))
res.aov2 <- aov(twoway$weight ~ twoway$day + twoway$worker, data = twoway)
summary(res.aov2)
#                Df Sum Sq Mean Sq F value   Pr(>F)    
#  twoway$day     1   6.08   6.084   4.934   0.0329 *  
#  twoway$worker  3  54.62  18.207  14.767   2.24e-06 ***
#  Residuals     35  43.15   1.233                     

# Box plot with two factor variables
boxplot(twoway$weight ~ twoway$day + twoway$worker, data = twoway, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Tooth Length")
# Two-way interaction plot
interaction.plot(x.factor = twoway$day , trace.factor = twoway$worker, 
                 response = twoway$weight, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Dose", ylab="Tooth Length",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))