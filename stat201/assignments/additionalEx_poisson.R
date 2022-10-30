#########################
# additional exercises  #
# Poisson regression    #
#########################

# crab data
# Horseshoe crabs arrive on the beach in pairs and
# spawn during high tides. Unattached males also 
# come to the beach, crowd around the nesting
# couples and compete with attached males for
#  fertilizations.  Satellite males  form large
#  groups around some couples while ignoring others,
#  resulting in a nonrandom distribution
#  that cannot be explained by local
#  environmental conditions or habitat selection.


# color: light, medium, dark, and darker
# spine: spine condition: good, middle, bad
# width: width in centimeters
# satell: number of satellites, which males clustering around
# the female in addition to the male with which she is
# breeding.
# weight: weight in grams.
# y: shorthand for as.numeric(satell > 0) (that is: 0 if
# female has no satellites, 1 if she has at least one)


### fit a log-linear model to answer the following
# question: how many satellite males  tend to
#  cluster around a female with specific features?
#  For this exercise, use "satell" as response!

##############################################
##############################################


# ALCOHOL, CIGARETEES AND MARIJUANA USE 
# A survey by the Wright State University School of Medicine 
# and the United Health Services in Dayton, Ohio. The survey
# asked 2276 students in their final year of high school in 
# a nonurban area near Dayton, Ohio, whether they had ever 
# used alcohol, marijuana or cigarettes
# variables are 
# M : marijuana (1 = yes, 2 = no)
# C: cigarettes  (1 = yes, 2 = no)
# A: alcohol  (1 = yes, 2 = no)

library(CatDataAnalysis)

data(table_9.3)
dat <- table_9.3

 
### a)
# fit a log-linear model with "count" as response variable.
# check the independence assumption.



### b) 
# fit the saturated model. Then, search for the
# best unsaturated model by
# including different pairwise interactions

##############################################
##############################################


# Government Spending Data
# Subjects in a GSS were asked their opinions about government
# spending on the environment (E), health (H), assistance to 
# big cities (C), and law enforcement (L). 
# Outcome categories are 
# 1 = too little
# 2 = about right
# 3 = too much


data(exercise_9.5)
dat <- exercise_9.5


### a)
# fit a log-linear model with "count" as response variable.
# check the independence assumption.


### b)
#can you find a nonsaturated model that fits the data well?

##########################################################
##########################################################

