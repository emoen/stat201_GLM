###########################
# NEWTON - RAPHSON METHOD #
###########################

library(dobson) # include the data in the DB book

# DB 1.6.5 uses a bisection method: here we use the NR method
dat <- cyclones
y.bar <- mean(dat$number)
n <- nrow(dat)

# NR iteration
lambda <- array()
lambda[1] <- 1 # initialization 
for(s in 2:10){
  lambda[s] <- lambda[s-1]*(2-lambda[s-1]/y.bar)
}
# updates approach MLE
unlist(lambda)
y.bar 
plot(unlist(lambda))
abline(h=y.bar)

# the likelihood function with and without constant
lik <- function(lambda) lambda^(n*y.bar) * exp(-n*lambda)/prod(dat$number)
plot(lik,0,10)
lik0 <- function(lambda) lambda^(n*y.bar) * exp(-n*lambda) 
plot(lik0,0,10,col=2,type="l")

# the log-likelihood function
log_lik <- function(lambda) y.bar*log(lambda)-lambda
der_loglik <- function(lambda) y.bar/lambda -1
plot(log_lik,0,10)
abline(v=y.bar)
points(unlist(lambda),log_lik(unlist(lambda)),pch=21,bg=1)
plot(der_loglik,0.1,10)
abline(h=0)
abline(v=y.bar)
points(unlist(lambda),der_loglik(unlist(lambda)),pch=21,bg=1)
