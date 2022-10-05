# stat201 Generalized Linear Models

Densities from the exponential family
```
x=seq(1,40,length=40)
plot( dnorm(x, mean=mean(x),sd=sd(x)  ), type="l")
```
<img src="../../blob/main/images/normal_density.png" width="200" height="400">

```
plot( dpois(x, mean(x)), type="l")
```
![dpois((1..40), mean=20)](../../blob/main/images/poisson_density.png)

```
plot( dexp(x, mean(x)), type="l" )
```
![dpois((1..40), mean=20)](../../blob/main/images/exponential_density_20.png)

```
x=seq(-5,10,length=40)
rate = mean(x)
plot( dexp(x, rate), type="l" )
```
![dpois((-5..10), mean=7.5)](../../blob/main/images/exponential_density_7.png)
