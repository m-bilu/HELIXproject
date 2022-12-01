###########################################
# This file is for data imputation and cleaning.
# MICE: Multiple Imputation with Chained Equations package
#install.packages('mice')
#install.packages('VIM')
library(mice)
library(VIM)

load("C:/Users/mbila/Downloads/exposome_NA.RData")

data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_data_v2.csv')




## --------------- DEFINING CLEANING PROCESS -------------- ##


### Methods of cleaning
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# https://www.youtube.com/watch?v=MpnxwNXGV-E
# https://www.theanalysisfactor.com/multiple-imputation-in-a-nutshell/


# What are we counting as dirty data? N/A's only.
# Which covariates have NA's?
na <- which(lapply((lapply(data, is.na)), function(x) {length(x[x==TRUE])}) > 0)
dataRmv <- na.omit(data) # all obvs with no NA Values
md.pattern(data[, names(na)])
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

## IS THIS ENOUGH TO KICK IT OUT?


## --------------- CLEANING OPTIONS -------------- ##

# List-wise Deletion: Removing rows with NA's, BIASED, WORST

# Mean/Mode Imputation: Replacing NAs with column mean/mode BIASED

# Multiple Imputation: Regressing NA covariate on rest, lowers Biasness

#   We need to prove MAR!!!!



## --------------- CLEANING -------------- ##








# --/ List-wise Deletion
toDel <- c()
for (i in 1:nrow(data)) {
  if (length(which(is.na(data[i, ]))) > 0) {
    toDel <- c(toDel, i)
  }
}
dataDel <- data[-toDel, ]
write.csv(dataDel, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v0.csv',
          row.names = FALSE)














# --/ Mean/Mode Imputation
#### STEP 1: Acquiring column means/modes
modes <- rep(0, ncol(data))

getmode <- function(v) {
  unique(v)[which.max(tabulate(match(v, unique(v))))]
}

for (col in 1:ncol(data)) {
  curmode <- NULL
  
  ## Now, vec has no null values. Calculate and assign means.
  vec <- data[, col]
  
  if (is.numeric(vec)) { 
    curmode <- mean(vec, na.rm = TRUE)
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

## Deleting weird X column (duplicate column of ID column):
cleandata <- cleandata[, -1]

write.csv(cleandata, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v1.csv',
          row.names = FALSE)

## Double check if u have all covariates, should be 241 not 237 
## NOTE: # graph with MSPE, phi, lambda















# --/ Multiple Imputation
dataImp <- mice(data, m=5, method='lasso.norm')
summary(dataImp)

# iterated values replacing NAs in hs_correct_raven dataset
dataImp$imp$hs_correct_raven

dataFinal <- complete(dataImp, 1)

## Deleting weird X column (duplicate column of ID column):
dataFinal <- dataFinal[, -1]

write.csv(dataFinal, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v2.csv',
          row.names = FALSE)



###############################################################
### ----------- Choosing Best Cleaning Strategy ----------- ###

## Measurement for quality of cleaning?
# Biasness of data?
# Variance of data?
# CHECK EMAIL



## NOTE: # graph with MSPE, phi, lambda















