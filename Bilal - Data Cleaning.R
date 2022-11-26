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
#     Mean/Mode Imputation
#     mean imputation does not apply to non-numeric columns


#### STEP 1: Acquiring column means/modes
modes <- rep(0, ncol(data))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (col in 1:ncol(data)) {
  curmode <- NULL
  
  ## Now, vec has no null values. Calculate and assign means.
  vec <- data[, col]
  
  if (is.numeric(vec)) { 
    curmode <- mean(vec, na.rm == TRUE)
  } else {
    curmode <- getmode(vec)
  }
  modes[col] <- curmode
}

## Now, modes is complete

#### STEP 2: Assigning all NA's to same node value
cleandata <- data

for (colnum in 1:ncol(cleandata)) {
  col <- cleandata[, colnum]
  for (rownum in 1:length(col)) {
    if (is.na(col[rownum])) {
      cleandata[rownum, colnum] <- modes[colnum]
      }
  }
}

## Now, all NA's within one column hold same mode value
## Sanity check:
which(is.na(data)) # Should be non-zero vector
which(is.na(cleandata)) # Should be 0

## Deleting wierd X column (duplicate column of ID column):
cleandata <- cleandata[, -1]

write.csv(cleandata, 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v1.csv')

## NOTE: # graph with MSPE, phi, lambda









