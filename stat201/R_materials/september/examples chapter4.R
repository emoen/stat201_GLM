########################
# example in 4.1 of DB #
########################

# we want to reporduce table 4.2

library(dobson)

dat <- failure
y <- dat$lifetimes

lambda <- 2
theta0 <- mean(y) # initial point
n <- length(y)
# comparing Newton-Raphson and Fisher scoring
# table 4.2 has four column: we do 10 
out <- array(dim=c(6,10))
theta <- theta0
for(m in 1:10){
  U <- (-2*n/theta + 2*sum(y^2)/theta^3)*10^6
  U_PRIME <- (2*n/theta^2-6*sum(y^2)/theta^4)*10^6
  E_U_PRIME <- (-4*n/theta^2)*10^6
  out[,m] <- c(theta, U, U_PRIME, E_U_PRIME,
               U/U_PRIME, U/E_U_PRIME)
  theta <- theta - U/U_PRIME
}
out

?dweibull

loglik <- function(theta){
   fun <- sum(dweibull(y,shape=2,theta,log=TRUE))
   return(fun)
}
ll <- list()
i=0
for(theta in 7000:14000){
  i=i+1
  ll[[i]] <- loglik(theta)
}
plot(7000:14000, unlist(ll),type="l",xlab="lambda",ylab="loglikelihood")
abline(v=out[1,10]) # the MLE found before


###############
# example 4.4 #
###############

# we fit a Poisson regression model by IWLS

dat <- poisson
n <- nrow(dat)
beta <- c(7,5)
X <- matrix(c(rep(1,n),dat$x),n,2)
y <- dat$y
x <- dat$x
for(i in 1:4){
  w <- as.vector(1/(X%*%beta)) #matrix multiply
  #mod <- lm( y ~ x, data = dat,weights = w)
  #beta <- coef(mod)
  bar_y <- sum(w*y)/sum(w)
  bar_x <- sum(w*x)/sum(w)
  beta[2] <- sum(w*(y-bar_y)*(x-bar_x))/(sum(w*(x-bar_x)^2))
  beta[1] <- bar_y - beta[2]*bar_x               
  print(beta)
}

# check with MLE
glm(y ~ x, data = dat, family=poisson(link="identity"))
