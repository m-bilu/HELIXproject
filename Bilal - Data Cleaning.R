###########################################
# This file is for data imputation and cleaning.

# What are we counting as dirty data?
# N/A values
# Incorrectly formatted? not found so far
# Incorrect data? Assuming all is correct
# Standardize indicator variables

# First we get subset of Exploratory variables

data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_data.csv')
  
# Now we select covariates of interest for our study
# All lifestyle-tagged covariates pertaining data on the mother, post/prenatally

### Methods of cleaning
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# - Regress covariates with missing data on all complete covariates,
#     predict missing data
# - Train model on Training Data (all complete), train








