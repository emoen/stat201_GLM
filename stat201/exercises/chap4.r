library(dobson)

data( aids )
summary( aids )

aids$column = paste(aids$year, aids$quarter)

#4.1 
#a) plot cases 
plot(factor(aids$column), aids$cases)

# b) poisson dist, plot log i, log y_i
n = length(aids$cases)
i = seq(1, n)
plot(log(i), log(aids$cases))

# c) fit GLM g(lambda_i)=log lambda_i=B_1+B_2*x_i, where x_i=log(i)
# By first principles: weight matrix W, and X^TWXb^(m)=X^TWz - validate with software
X = matrix(cbind(rep(1, n), seq(1,n)),nrow=n, ncol=2)
y <- aids$cases
W = diag(1, nrow=n, ncol=n)

t(X)%*%W%*%X

######################
setwd("C:/prosjekt/uib/stat201/datasets")
dat <- read.table("gamma regression data.txt")
######################

# IWLS iteration
beta <-c(-14,7.7)
beta.seq <- array(dim=c(n,2))
for(step in 1:n) {
  
  eta<- X%*%beta  
  z <- eta + (y-exp(eta))/exp(eta)
  beta.seq[step,] <- solve(t(X)%*%X)%*%t(X)%*%z
  beta <- beta.seq[step,] 
}
beta.seq  

aids$qs = seq(1,n)
an = glm(aids$cases ~ aids$qs, family = gaussian, data = aids)
#(Intercept)      aids$qs  
# -14.811        7.653  
summary(an)

tmp = function(){

  alm = lm(aids$cases ~ aids$qs, data = aids)
}

plot(aids$qs, aids$cases)
lines(predict(an, newdata=aids, type="response"))
abline(-7.9, 7)
lines


#####################################################
##### Iterated Weighted Least Square ################
#####################################################
#https://towardsdatascience.com/iterated-reweighted-least-squares-and-glms-explained-9c0cc0063526
y = aids$cases
mu = c(0.5, 0.5)

delta = 1
i=0
LL=0

while (delta > 10^-6){
  # 1. Given current μ, calculate z and W
  z = mu + (y-mu)*(1) #g(mu), dg(mu)=1 - 
  W = diag(1/ X[,2]) #  W = np.diag(1 / (derivativeOfg(mu)**2 * Var(mu))) - link: g(x)=mu, var(x)=x
  
  # 2. Given current z and W, calculate β
  #b = WLS(X, W, z)
  # Normal equations of weighted least squares
  XtWX = t(X) %*% W %*% X
  XtWz = t(X) %*% W %*% z
  b = solve(XtWX) %*% XtWz # XtWX^-1
  
  # 3. Given current β, calculate μ
  mu = X %*% b # ig() Inverse link function
  
  # 4. Calculate and compare log-likelihoods
  LLOld = LL
  p = mu
  #LL = sum( y * log(p) + (1 - y) * log(1 - p) ) # Log-likelihood of logistic poisson regression
  LL = sum( -1 + y/mu)
  delta = abs(LL - LLOld)  
  
  i = i + 1
}

#B = -7.945712, 6.999592



