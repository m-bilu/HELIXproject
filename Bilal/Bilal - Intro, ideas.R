load("C:/Users/mbila/Downloads/exposome_NA.RData")



##################################################
###################################################

# THIS IS ALL OLD STUFF, OUTDATED
# ONLY ON REPO FOR RECORDING PURPOSES

##################################################
##################################################



## Data Summary:
# codebook explains all variables in data
# 3 datasets:
# phenotypeNA holds health outcomes on measured on children at birth/6-11
# exposomeNA holds environmental exposures of interest (main covariates),
#   measured on the mother during preg/postnatally
# covariatesNA holds covariates for either (extra)

# ID represents unique key for mother-child pair (out of order in exposomeNA)

#### GOALS:
# - Answer specific question about dataset
# - IMO use preset questions for efficiency
# - MY OUTCOMES OF INTEREST (y's in models) must come from phenotypeNA dataset

### Goals for exploratory data analysis:
# - summary statistics, histograms, scatterplots, comment on interesting 
# - findings, how they inform further analysis

# CURRENT QUESTION: building the best possible predictive model for birthweight
# what does exploring mean here?

# ASSUMPTIONS:
# - data is formatted correctly, is correctly corrected
# - All selected covariates pertain to the mother, not the child

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
# Data cleaning (in seperate file)
