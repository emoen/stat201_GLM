############################################
# additional exercises survival regression #
############################################

library(flexsurv)
library(KMsurv)

################
# larynx data  #
################

# load the data
data(larynx)
?larynx # look at the meaning of the variables

larynx$stage <- as.factor(larynx$stage) 

# ex 1 
# fit a weibull AFT model by including stage and age
# interpret the output

# ex 2
# plot the 4 survival curves estimated by the model
# of subjects with age = mean(age) 

# ex 3 
# include an interaction effect: do you obtain a better model?

#########################################
#########################################

data(alloauto)

#This study compares the efficacy of autologous (auto) versus 
# allogeneic (allo) transplants for acute myelogenous leukemia. 
# The outcome for the 101 patients was leukemia-free survival. 
# All patients in the sample were in their first complete 
# remission at least one year.

# ex 1 
# Fit an AFT Weibull regression model

# ex 2
# Compare the two type-specific cumulative hazards 
# estimated by the model to the nonparametric estimate 
# of the cumulative hazards 
# obtained by the two group-specific KM survival curves


#########################################
#########################################

data(bfeed)

# The National Longitudinal Survey of Youth is a stratified 
# random sample which was begun in 1979. Youths, aged 14 to 21
# in 1979, have been interviewed yearly through 1988. Beginning
# in 1983, females in the survey were asked about any 
# pregnancies that have occurred since they 
# were last interviewed (pregnancies before 1983 were also 
# documented).
# Questions regarding breast feeding are included in the 
# questionnaire. This data set consists of the information 
# from 927 first-born children
# to mothers who chose to breast feed their children and 
# who have
# complete information for all the variables of interest. 
# The sample was
# restricted to children born after 1978 and whose gestation age was
# between 20 and 45 weeks. The year of birth restriction was included in
# an attempt to eliminate recall problems.
# The response variable in the data set is duration of breast feeding
# in weeks, followed by an indicator of whether the breast feeding was
# completed (i.e., the infant is weaned). 
# Explanatory variables for breastfeeding
# duration include 
# race of mother (1 if white, 2 if black, 3 if other);
# poverty status indicator (1 if mother in poverty); 
# smoking status of mother (1 if smoking at birth of child);
# alcohol-drinking status of mother (1 if drinking at birth of child); 
# age of mother at child's birth,
# year of child's birth, 
# education of mother (in years); 
# lack of prenatal care status (1 if mother sought prenatal care after third month or never
#             sought prenatal care, 0 if mother sought prenatal care in first three
#             months of pregnancy). 

# Fit all the parametric  models that we have considered in this
# course a make a proposal

