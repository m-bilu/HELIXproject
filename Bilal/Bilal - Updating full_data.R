##################
## Creating full_data as union of covariatesNA, exposomeNA, hs_correct_raven
##    First, we must sort data for ID
load("C:/Users/mbila/Downloads/exposome_NA.RData")

covariatesNASorted <- covariatesNA[order(covariatesNA$ID), ]
correctRavenSorted <- phenotypeNA[order(phenotypeNA$ID), c('ID', 'hs_correct_raven')]
exposomeNASorted <- exposomeNA[order(exposomeNA$ID), ]

full_data <- cbind(correctRavenSorted, covariatesNASorted[, -1], exposomeNASorted[, -1])

## Writing data out into csv:
write.csv(full_data, 'C:/Users/mbila/Documents/STAT 331 Final Project/full_data_v2.csv')