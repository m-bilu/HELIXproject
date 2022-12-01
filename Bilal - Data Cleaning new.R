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
# => 17.7% of full data missing, we can use imputation without negative effects
# => We are setting this as our threshold.
# => ASK MCGEE IF THIS IS OKAY


## --------------- NOTE FOR EDA -------------- ##
# Only one covariate thats causing % of data to go really high
# It is hs_wgtgain_None (12% of data missing)
# What to do with this?

# Visualizing NA columns (EDA):
aggr_plot <- aggr(data, col=c('navyblue', 'red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("HISTOGRAM OF MISSING DATA", "PATTERN"))

## --------------- CLEANING -------------- ##
dataImp <- mice(data, m=5, method='lasso.norm')
summary(dataImp)

# iterated values replacing NAs in hs_correct_raven dataset
dataImp$imp$hs_correct_raven

dataFinal <- complete(dataImp, 1)

write.csv(dataFinal, file = 'C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v2.csv',
          row.names = FALSE)


## NOTE: # graph with MSPE, phi, lambda









