# FIXING full_data to include covariatesNA data as well
load("C:/Users/mbila/Downloads/exposome_NA.RData")

full_data <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_data.csv')

full_data <- cbind(full_data, covariatesNA[, 2:ncol(covariatesNA)])

## Checking full_data is correct
intersect(colnames(full_data), colnames(covariatesNA)) == colnames(covariatesNA)
# intersect(full_data, covariatesNA) == covariatesNA

##  Checking how many are lifestyle 
##    (this may mean we need to update other datasets as well)
lifestyleSubset <- codebook[codebook$family == 'Lifestyle', ]
intersect(lifestyleSubset, covariatesNA)


##  Double check concludes that no lifestyle covariates are in covariateNA,
##    => Only need to update full_data
##    Now we export
write.csv(full_data, 'C:/Users/mbila/Documents/STAT 331 Final Project/full_data.csv')
