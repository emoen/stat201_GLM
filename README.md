# stat201 Generalized Linear Models

Densities from the exponential family
'''
x=seq(1,40,length=40)
plot( dnorm(x, mean=mean(x),sd=sd(x)  ), type="l")
'''

'''
plot( dpois(x, mean(x)), type="l")
'''

'''
plot( dexp(x, mean(x)), type="l" )
'''

'''
x=seq(-5,10,length=40)
rate = mean(x)
plot( dexp(x, rate), type="l" )
'''
