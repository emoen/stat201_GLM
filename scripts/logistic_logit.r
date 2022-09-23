aseq = seq(-10, 10, by=0.1)
logistic = function(a=0, b=1, x=seq(0,1,by=0.1)) { return ( exp(a+b*x)/(1+exp(a+b*x)) ) }
logit = function(a=0, b=1, x=seq(0,1,by=0.1)) { return ( exp(a+b*x)/(1+exp(a+b*x)) ) }


plot(aseq, logistic(0, 1, aseq))
plot(aseq, logit())
