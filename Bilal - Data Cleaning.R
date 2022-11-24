###########################################
# This file is for data imputation and cleaning.
load("C:/Users/mbila/Downloads/exposome_NA.RData")

# What are we counting as dirty data?
# N/A values
# Incorrectly formatted? not found so far
# Incorrect data? Assuming all is correct
# Standardize indicator variables

# First we get subset of Exploratory variables

data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_data_v2.csv')

### Methods of cleaning
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# - Regress covariates with missing data on all complete covariates,
#     predict missing data
# - Train model on Training Data (all complete), predict missing values
# - CURRENT STRATEGY: 
#     Mode Imputation (so numeric, non-numeric columns have same strategy)
#     mean imputation does not apply to non-numeric columns


#### STEP 1: Acquiring column modes
modes <- rep(0, ncol(data))

for (col in 1:ncol(data)) {
  curmode <- mode(data[, col])
  modes[col] <- curmode
}

## Now, modes is complete

#### STEP 2: Assigning all NA's to same node value
colnum <- 0
cleandata <- sapply(data, function(col) {
  colnum <- colnum + 1
  for (i in 1:length(col)) {
    col[i] = modes[colnum]
  }
})

## Now, all NA's within one column hold same mode value
## Sanity check:
which(is.na(cleandata)) # Should be 0
which(is.na(data)) # Should be non-zero vector

write.csv(full_data, 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v1.csv')









## NOTE: # graph with MSPE, phi, lambda









