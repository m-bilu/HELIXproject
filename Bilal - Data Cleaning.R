###########################################
# This file is for data imputation and cleaning.
load("C:/Users/mbila/Downloads/exposome_NA.RData")

# What are we counting as dirty data?
# N/A values
# Incorrectly formatted? not found so far
# Incorrect data? Assuming all is correct
# Standardize indicator variables

# First we get subset of Exploratory variables

data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_data.csv')


### Methods of cleaning
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# - Regress covariates with missing data on all complete covariates,
#     predict missing data
# - Train model on Training Data (all complete), predict missing values
# - For now, we replace with mean value, document what this does to variance,
#     why this is appropriate option.









## NOTE: # graph with MSPE, phi, lambda









