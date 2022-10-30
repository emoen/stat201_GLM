# stat201 Generalized Linear Models

Densities from the exponential family

The theory for linear normal models is looked at and applied to regression and analysis of variance. Furthermore the topics of binary variables logistic regression, log-linear models, contingency tables and life time analysis are treated.

Learning Outcomes
After completed course, the students are expected to be able to:

-Identify probability distributions belonging to an exponential family and adapt a description as a generalized linear model. <\br>
-Present the general theory of exponential families of distributions.<\br>
-Describe numerical procedures for estimation in generalized linear models.<\br>
-Recognize linear normal models and apply general test procedures to these models.<\br>
-Explain the proofs of important theorems in probability theory utilized in test procedures in linear normal models and in generalized linear models.<\br>
-Analyze data sets following Poisson or binomial distributions.<\br>
-Estimate parameters and test hypotheses in generalized linear models by means of statistical software.<\br>

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
<img src="../../blob/main/images/exponential_density_7.png" width="300" height="300">
