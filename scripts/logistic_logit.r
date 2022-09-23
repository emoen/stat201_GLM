aseq = seq(-10, 10, by=0.1)
logistic = function(a=0, b=1, x=aseq) { return ( exp(a+b*x)/(1+exp(a+b*x)) ) }
aseq = seq(0, 1, by=0.01)
logit = function(x=aseq) { return ( log(x/(1-x)) ) }


plot(aseq, logistic(0, 1, aseq))
plot(aseq, logit())
