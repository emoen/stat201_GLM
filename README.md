# stat201 Generalized Linear Models

Densities from the exponential family
```
x=seq(1,40,length=40)
plot( dnorm(x, mean=mean(x),sd=sd(x)  ), type="l")
```
<img src="../../blob/main/images/normal_density.png" width="300" height="300">

```
plot( dpois(x, mean(x)), type="l")
```
<img src="../../blob/main/images/poisson_density.png" width="300" height="300">

```
plot( dexp(x, mean(x)), type="l" )
```
<img src="../../blob/main/images/exponential_density_20.png" width="300" height="300">

```
x=seq(-5,10,length=40)
rate = mean(x)
plot( dexp(x, rate), type="l" )
```
<img src="../../blob/main/images/images/exponential_density_7.png" width="300" height="300">
