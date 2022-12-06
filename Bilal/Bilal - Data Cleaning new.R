###########################################
# This file is for data imputation and cleaning.
# MICE: Multiple Imputation with Chained Equations package
#install.packages('mice')
#install.packages('VIM')
library(mice)
library(VIM)

load("C:/Users/mbila/Downloads/exposome_NA.RData")

data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/Data/full_data_v2.csv')




## --------------- DEFINING CLEANING PROCESS -------------- ##


### Methods of cleaning
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# https://www.youtube.com/watch?v=MpnxwNXGV-E
# https://www.theanalysisfactor.com/multiple-imputation-in-a-nutshell/
# https://www.linkedin.com/pulse/mice-nice-why-should-you-care-ofir-shalev/


# What are we counting as dirty data? N/A's only.
# Which covariates have NA's?
dataRmv <- na.omit(data) # all obvs with no NA Values
dataColNames <- data
colnames(dataColNames) <- (1:ncol(dataColNames))
na1 <- which(lapply((lapply(dataColNames, is.na)), function(x) {length(x[x==TRUE])}) > 0)
na2 <- which(lapply((lapply(data, is.na)), function(x) {length(x[x==TRUE])}) > 0)
md.pattern(dataColNames[, names(na1)], rotate.names = TRUE)
nrow(data) - nrow(dataRmv) 

# 230 rows with some NA's
# 104 total values missing out of 1301*238 values
# => 17.7% of full data missing
# => 11.8% of missing rows contain NA for hs_wgtgain_None (154 rows)
wgt <- data[, 'hs_wgtgain_None']
length(wgt[!is.na(wgt)])

# Visualizing NA columns (EDA):
aggr_plot <- aggr(data, col=c('navyblue', 'red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("HISTOGRAM OF MISSING DATA", "PATTERN"))


## ------------ REMOVING WGT COVARIATE ------------ ##
## HOW to justify removing covariate?

# 1) Is distribution of NA's random or dependent on other covariates?
# 2) wgt is not related to raven score

# Plotting relationship between hs_correct_raven and wgt's complete values
toDel <- c()
for (i in 1:length(wgt)) {
  if (is.na(wgt[i])) {
    toDel <- c(toDel, i)
  }
}
dataShort <- data[-toDel, ]
wgtShort <- wgt[-toDel]

plot(wgtShort, dataShort[, 'hs_correct_raven']
     , ylab='Raven Score', xlab=codebook['hs_wgtgain_None', 'description'])

## No visible relationship in between Raven Score and Maternal Weight Gain
##  during pregnancy. No slope, no clustering of datapoints depending on 
##  weightgain value.


## --------------- CLEANING OPTIONS -------------- ##

# List-wise Deletion: Removing rows with NA's, BIASED, WORST

# Mean/Mode Imputation: Replacing NAs with column mean/mode BIASED

# Multiple Imputation: Regressing NA covariate on rest, lowers Biasness

#   We need to prove MAR!!!!


## BEFORE ANYTHING, must check for wierd catagorical covariates which 
# are stored as numerical


## --------------- CONVERTING CATEGORICAL COVARIATES -------------- ##


## Categorical Covariates labelled as numeric, Removing ID/X columns
if (length(which(colnames(data) == c('X', 'ID'))) != 0) {
  data <- data[, -which(colnames(data) == c('X', 'ID'))]
}

# HELPER - Checking if function is true for all elements in a vector
floorCheck <- function(x) {
  for (i in 1:length(x)) {
    if (floor(x[i]) != x[i]) {
      return (FALSE)
    }
  }
  return (TRUE)
}

whichNumeric <- c()
for (i in 1:ncol(data)) {
  vec <- data[, i]
  if (is.numeric(vec)) {
    whichNumeric <- c(whichNumeric, i)
  }
}
dataNumeric <- data[, whichNumeric]
whichSus <- c()
for (i in 1:ncol(dataNumeric)) {
  vec <- dataNumeric[, i]
  vecNoNA <- na.omit(vec)
  if (floorCheck(vecNoNA)) {
    whichSus <- c(whichSus, i)
  }
}
dataSus <- na.omit(dataNumeric)[, whichSus]

# Ranges for each suspicious column
lapply(dataSus, range)

# Columns in dataSus are labelled as numeric, but are actually categorical
# SOLUTION: Replace numeric values with factored versions to 
#   convert vector into categorical

# After modification of data, columns will be added back to original data
dataFinal <- data
whichFix <- which(colnames(data) %in% colnames(dataSus))
for (i in 1:ncol(data)) {
  if (i %in% whichFix) {
    dataFinal[, i] <- as.factor(dataFinal[, i])
  }
}



## --------------- CLEANING -------------- ##


# --/ List-wise Deletion
toDel <- c()
for (i in 1:nrow(dataFinal)) {
  if (length(which(is.na(dataFinal[i, ]))) > 0) {
    toDel <- c(toDel, i)
  }
}
dataFinalDel <- dataFinal[-toDel, ]
#write.csv(dataFinalDel, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/Data/full_clean_data_v0.csv',
#          row.names = FALSE)








# --/ Mean/Mode Imputation
#### STEP 1: Acquiring column means/modes
modes <- rep(0, ncol(dataFinal))

getmode <- function(v) {
  unique(v)[which.max(tabulate(match(v, unique(v))))]
}

for (col in 1:ncol(dataFinal)) {
  curmode <- NULL
  
  ## Now, vec has no null values. Calculate and assign means.
  vec <- dataFinal[, col]
  
  if (is.numeric(vec)) { 
    curmode <- mean(vec, na.rm = TRUE)
  } else {
    curmode <- getmode(vec)
  }
  modes[col] <- curmode
}

## Now, modes is complete

#### STEP 2: Assigning all NA's to same node value
cleandataFinal <- dataFinal

for (colnum in 1:ncol(cleandataFinal)) {
  col <- cleandataFinal[, colnum]
  for (rownum in 1:length(col)) {
    if (is.na(col[rownum])) {
      cleandataFinal[rownum, colnum] <- modes[colnum]
    }
  }
}

## Now, all NA's within one column hold same mode value
## Sanity check:
which(is.na(dataFinal)) # Should be non-zero vector
which(is.na(cleandataFinal)) # Should be 0

#write.csv(cleandataFinal, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/Data/full_clean_data_v1.csv',
#          row.names = FALSE)

## Double check if u have all covariates, should be 241 not 237 
## NOTE: # graph with MSPE, phi, lambda








# --/ Multiple Imputation
# https://www.section.io/engineering-education/predictive-mean-matching/#solving-for-missing-values-using-predictive-mean-matching
  
dataImp <- mice(dataFinal, m=5, method='pmm')
summary(dataImp)

# Checking Imputations for each column
dataImp$imp$hs_correct_raven

## PICKING BEST COLUMN
# For each imputed column, we replace column with
# one of the 5, one with best imputation

# JUSTIFY CHOOSING 5
dataF <- complete(dataImp, 5)

#write.csv(dataF, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/Data/full_clean_data_v2.csv',
#          row.names = FALSE)



###############################################################
### ----------- Choosing Best Cleaning Strategy ----------- ###

# IF data follows MAR assumption, Multiple imputation is best
# Checking algorithm from 
# https://campus.datacamp.com/courses/scalable-data-processing-in-r/case-study-a-preliminary-analysis-of-the-housing-data?ex=4#:~:text=To%20check%20if%20your%20data,meaning%20your%20data%20are%20MAR.
#   Must be performed for all columns with some NA's

# Algorithm described in report
# We are assuming that data is not MNAR, since we are not in contact with
#   data collection team, cannot discern dependencies of covariates 
#   outside of observed data.


binarize <- function(vec) {
  res <- rep(0, length(vec))
  for (i in 1:length(vec)) {
    if (is.na(vec[i])) {
      res[i] = 1
    }
  }
  
  return (res)
}

dataProof <- data[, colSums(is.na(data)) > 0]

pvalMeans <- rep(NA, ncol(dataProof))

for (i in 1:ncol(dataProof)) {
  curcol <- dataProof[, i]
  bin <- binarize(curcol)
  
  dataDep <- dataProof[, -i] # All dependent covariates, ith cov is indep
  
  pvals <- rep(NA, ncol(dataDep))
  
  for (j in 1:ncol(dataDep)) {
    # Logistic regression
    Mtest <- summary(glm(bin ~ dataDep[, j]),
                     family=binomial)
    pvals[j] <- Mtest$coefficients[2, 4]
    
  }
  
  pvalMeans[i] <- mean(pvals)
  
  print(paste('i =', i))
  
}

median(na.omit(pvalMeans))















