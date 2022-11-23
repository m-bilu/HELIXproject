load("C:/Users/mbila/Downloads/exposome_NA.RData")

# CURRENT QUESTION: How well do mother's lifestyle covariates predict child's
# Raven Test score. Also, which (out of postnatal and prenatal) affecct MORE?

##############################################

# Possible outcome variables in phenotypeNA:
summary(phenotypeNA)
# ID: representing unique mother-child pair
# e3_bw: CHILDWEIGHT AT BIRTH
codebook['hs_asthma', ]
# e3_bw: Body mass index z-score at 6-11 years old
codebook['hs_zbmi_who', ]
# e3_bw: Total of correct answers at the RAVEN test
codebook['hs_correct_raven', ]
# e3_bw: Neuro Behaviour
codebook['hs_Gen_Tot', ]
# e3_bw: BMI at 6-11 yrs old
codebook['hs_bmi_c_cat']

##############################################
#### GOALS OF EDA
# Summarize our dataset
# Visualize dataset
# Identify missing values and come up with a cleaning strategy


## 1) Summary

