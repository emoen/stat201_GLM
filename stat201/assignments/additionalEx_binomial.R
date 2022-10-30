#########################
# additional exercises  #
# logistic regression   #
#########################

# CRAB DATA
# Horseshoe crabs arrive on the beach in pairs and
# spawn during high tides. Unattached males also come to the 
# beach, crowd around the nesting
# couples and compete with attached males for fertilizations. Satellite males 
# form large groups around
# some couples while ignoring others, resulting in a nonrandom distribution
#  that cannot be explained
# by local environmental conditions or habitat selection.

# The main research question is whether satellite males
# are attracted by a female with specific features 

# the data is contained in the file "crab.txt"

# color: light, medium, dark, and darker
# spine: spine condition: good, middle, bad
# width: width in centimeters
# weight: weight in grams.
# y: whether female has at least one satellite
# We are not interested in the variable "satell"


##### a) ------------
## Fit a logistic model with y as response variable and 
## all other variables as explanatory (except for "satell") and
## no interaction terms. What do you observe?
## Afterwards, compute the correlation of the variables "width" and
## "weight". How do you think this result may affect the analysis?



#### b) ----------
## Try different combinations of response variables, and 
## select the model that best fits the data. Use the function
## "predict" to produce a plot of the best logistic regression.



### c) ------
## Try different link functions. Do you obtain a better fit? 
## Plot the best regression line you can find in point c) against
## the best fitting line from point b).



######################################################
######################################################

# HEART DATA
# A sample of subjects were classified according their blood pressure
# at baseline and to development of 
# coronary heart disease after a 6-year follow-up
# The data is contained in the file "heart.txt"

#### a) ----
## Fit a null model, a saturated model (treating bp as a factor),
## and a "linear" model (trating bp as a continuous variable).
## Compare the models with the appropriate tests.



### b) -----
## fit a quadratic model (that is, use both bp and bp^2 as explanatory
## variables). Test the goo fit of the model.
## Then, plot the linear model from point a) against the quadratic model



################################################
################################################

# HEART DATA (WITH BLLOD PRESSURE AND SERUM CHOLESTEROL) 
# The dataset "heart_Bp_Chol.txt" contains information both
# on blood pressure and cholesterol. 

### a) -----
## Perform a similar analysis to the previous exercise, but 
## this time add chol as explanatory variable. Investigate 
## an eventual interaction effect.



################################################
################################################

####CREDIT CARD DATA

# very old study: 
# for each level of annual income (in old Italian lira)
# the number y of subjects possessing a credit card among
# the n subjects with that income level 

## install the package "CatDataAnalysis" and load the dataset
## exercise_5.22


library(CatDataAnalysis)
data(exercise_5.22)
dat <- exercise_5.22


### a) -----
## Investigate a logistic model that describes y using income
## as explanatory variables. Plot the model; what are your conclusions?


