################################
################################
#### PURPOSE:

## Performing step-wise and modified elastic net (seperately) to calculate
##    best variable selection for hs_correct_raven response variable. Comparing
##    both processes, gauging performance, eliminating high pval covariates,
##    selecting covariates/interactions based on scatterplots 
##    -->
##    deciding on final model before moving on to final Beta estimation with OLS, 
##    model validation with leave-one-out cross val.

## Loading in models, final cleaned data
## NOTE: for final calculations, use final cleaned data 
##    (with proper cleaning strategy)



 ## ----------- Data Splitting into Train, Validate, Test ------------- ##

library(ggplot2)
load("C:/Users/mbila/Downloads/exposome_NA.RData")
fullData <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/Data/model_selection_train.csv')
validData <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/Data/validation_final.csv')
testData <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/Data/test_final.csv')

if (length(which(colnames(fullData) == 'X')) > 0) {
  fullData <- fullData[, -which(colnames(fullData) == 'X')]
}

if (length(which(colnames(fullData) == 'X.1')) > 0) {
  fullData <- fullData[, -which(colnames(fullData) == 'X.1')]
}

if (length(which(colnames(validData) == 'X')) > 0) {
  validData <- validData[, -which(colnames(validData) == 'X')]
}

if (length(which(colnames(validData) == 'X.1')) > 0) {
  validData <- validData[, -which(colnames(validData) == 'X.1')]
}

if (length(which(colnames(testData) == 'X')) > 0) {
  testData <- testData[, -which(colnames(testData) == 'X')]
}

if (length(which(colnames(testData) == 'X.1')) > 0) {
  testData <- testData[, -which(colnames(testData) == 'X.1')]
}


#### Modifying all data to have hs_contactfam_3cat_num_None, 
# hs_cotinine_mcat_None to have categories with no spaces
fullData[, 'hs_contactfam_3cat_num_None'] <- gsub(' ', '_', fullData[, 'hs_contactfam_3cat_num_None'])
fullData[, 'hs_cotinine_mcat_None'] <- gsub(' ', '_', fullData[, 'hs_cotinine_mcat_None'])

validData[, 'hs_contactfam_3cat_num_None'] <- gsub(' ', '_', validData[, 'hs_contactfam_3cat_num_None'])
validData[, 'hs_cotinine_mcat_None'] <- gsub(' ', '_', validData[, 'hs_cotinine_mcat_None'])

testData[, 'hs_contactfam_3cat_num_None'] <- gsub(' ', '_', testData[, 'hs_contactfam_3cat_num_None'])
testData[, 'hs_cotinine_mcat_None'] <- gsub(' ', '_', testData[, 'hs_cotinine_mcat_None'])

## Add underscores to all spaces in covariates stored as strings


 
 ## -------------- Subjective: Covariate Transformation --------------- ##



# First, lets analyze the relationship in between outcome and covariates,
#   lets find covariates that need transformation
#   IE they have quadratic, logarithmic, etc. relationship with outcome

# Graphs for interaction terms:
# Aania Looking through lifestyle covs, remove then graph
lifeStyle <- as.character(codebook[codebook$family=='Lifestyle', 'variable_name'])
lifeStyle <- c(lifeStyle, 'hs_correct_raven')
lifeStyleInd <- which(colnames(fullData) %in% lifeStyle)
dataNoLife <- fullData[, -lifeStyleInd]

size <- 3
par(mfrow = c(size, size))
par(mar = c(4, 4, 3, 2))

plotTotal <- ceiling(ncol(dataNoLife)/(size*size)) # number of saved files
plotNum <- 1


pdf('C:/Users/mbila/Documents/STAT 331 Final Project/Bilal/outcomeGraphs/graph1.pdf')

for (i in 1:ncol(dataNoLife)) {
  
  # IF categorical/discrete var, draw boxplot
  if ((is.numeric(dataNoLife[, i]) == FALSE) 
      || length(which(floor(dataNoLife[, i]) == dataNoLife[, i]))
      == length(dataNoLife[, i])) {
      
    boxplot(fullData$hs_correct_raven~dataNoLife[, i], 
              ylab='Raven Score', 
              xlab=colnames(dataNoLife)[i], 
              main='Boxplot', ann=TRUE)
    
    # if continuous, draw scatterplot
  } else {
    
    plot(fullData$hs_correct_raven~dataNoLife[, i], 
           ylab='Raven Score', 
           xlab=colnames(dataNoLife)[i], 
           main='Plot', ann=TRUE)
  }
}

dev.off()

## PDF Stored
## List of subjective covs and transformation ideas

sometrans <- data.frame('e3_yearbir_None', 'quadratic')
names(sometrans) <- c('Covariate', 'Relationship')
add <- data.frame('h_parity_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_child_age_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_c_height_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_c_weight_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_pm25_yr_hs_h_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_landuseshan300_h_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_landuseshan300_s_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('h_TEX_Log', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_cs_c_Log2', 'quadratic') # + 5 to remove 0 values for log
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('h_humidity_preg_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('h_temperature_preg_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_hum_mt_hs_h_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_tm_mt_hs_h_None', 'logarithmic') # + 10
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_hum_wk_hs_h_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_tm_dy_hs_h_None', 'logarithmic')# + 20
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_tm_wk_hs_h_None', 'logarithmic')# + 10
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_pcb118_madj_Log2', 'logarithmic')# + 5
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_pcb170_madj_Log2', 'logarithmic') # + 5
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)
add <- data.frame('hs_hm_pers_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
sometrans <- rbind(sometrans, add)

# sometrans now holds the names and relationships of all covs that require transformation 

# Plotting for EDA

sometransNames <- sometrans[, 1]

for (i in sometransNames) {
  
  png(paste('C:/Users/mbila/Documents/STAT 331 Final Project/Bilal/outcomeGraphs/individual graphs/', i, '.png'))
  print(i)
  # IF categorical/discrete var, draw boxplot
  if ((is.numeric(dataNoLife[, i]) == FALSE) 
      || length(which(floor(dataNoLife[, i]) == dataNoLife[, i]))
      == length(dataNoLife[, i])) {
    
    print(i)
    
    boxplot(fullData$hs_correct_raven~dataNoLife[, i], 
            ylab='Raven Score', 
            xlab=i, 
            main='Boxplot', ann=TRUE)
    
    # if continuous, draw scatterplot
  } else {
    
    print(i)
    
    plot(fullData$hs_correct_raven~dataNoLife[, i], 
         ylab='Raven Score', 
         xlab=i, 
         main='Plot', ann=TRUE)
  }
  
  dev.off()
  
}






  ## -------------------- Subjective: Cov Interaction --------------------- ##





#### Let's print cov descriptions
listref <- codebook[which(codebook$family != 'Lifestyle'), c('description')]
list <- as.character(codebook[which(codebook$family != 'Lifestyle'), c('variable_name')])
# blood content during pregnancy may be connected
inter1 <- dataNoLife[, list[1:4]]
# blood content during hs test may be connected
inter2 <- dataNoLife[, list[5:16]]
# transport mode lines, mode stops, building/connectivity density during preg
inter3 <- dataNoLife[, list[c(17:30, 176:178)]]
# transport mode lines, mode stops, building/connectivity density after preg
inter4 <- dataNoLife[, list[c(31:40, 179:180)]]
# Concentration of content in child, mother
inter5 <- dataNoLife[, list[c(41:65, 181:183)]]
# Weather measurements during preg/month/day before at home
inter6 <- dataNoLife[, list[c(66:77)]]
# bluespace, greenspace, vegetation
inter7 <- dataNoLife[, list[c(78:86)]]
# Adjusted for lipids/creatine (various chemicals and compounds)
inter8 <- dataNoLife[, list[c(87:120)]]
# Sulfates in mother/child ## TOO BIG
inter9 <- dataNoLife[, list[c(121:166)]]
# Social Standings
inter10 <- dataNoLife[, list[c(167:175, 193:195)]]
# Status about birth, family
inter11 <- dataNoLife[, list[c(176:196)]]

# Now on all subsets of covariates with possible interactions, 
# let's check pvalues for interactions, selecting those with pval < 0.1

inters <- c()

interCheck <- aov(data = inter1, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter1)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter2, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter2)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter3, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter3)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter4, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter4)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter5, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter5)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter6, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter6)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter7, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter7)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter8, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter8)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

## Replace all 'Undetected's with ''
inters <- gsub('Undetected', '', inters)

interCheck <- aov(data = inter9, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter9)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter10, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter10)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

interCheck <- aov(data = inter11, formula = fullData$hs_correct_raven ~ .^2)
summary(interCheck)
ncolumn <- ncol(inter11)
inters <- c(inters, 
            (names(interCheck[[1]])
             [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))])
            [(which(summary(interCheck)[[1]][["Pr(>F)"]]
                    [(ncolumn+1):(length(summary(interCheck)[[1]][["Pr(>F)"]]))] < 0.1)+1)])

## Replace all 'Undetected's with ''
inters <- gsub('Less_than_once_a_week', '', inters)
inters <- gsub('SHS_smokers', '', inters)
inters <- gsub('Smokers', '', inters)
inters <- gsub('neither', '', inters)
inters <- gsub('Once_a_week', '', inters)
inters <- gsub('Low', '', inters)
inters <- gsub('Middle', '', inters)
inters <- gsub('male', '', inters)
inters <- gsub('female', '', inters)




#### NOW, inters holds all covariates worthy of interaction


  ## ------------------------- FORWARD Selection -------------------------- ##


## Using fwd so we dont need to mention upper model
## upper model would include all main effects and interactions,
## that is 56882 covariates in one model.

## IMPORTANT NOTE:
## Quadratic terms are only relevant for continuous covariates
fullData_cont <- fullData[, which(sapply(fullData, is.numeric))]
inputStr <- paste(' I(', colnames(fullData_cont)[colnames(fullData_cont)!='hs_correct_raven'] , "^2) +", collapse='') 

# Defining worst/best model for step function
M0 <- lm(hs_correct_raven ~ 1, data = fullData)
Mfull <- lm(hs_correct_raven ~ ., data = fullData)


 ## -------------------------------------------------------------------


## LEVEL 1: Main effects only, from all covariates in database
system.time({ 
  
  Mfwd1 <- step(object = M0, # base model
               scope = list(lower = M0, upper = Mfull),
               trace = 1, # trace prints out information
               direction = "forward")
  
})

length(unique(rownames(summary(Mfwd1)$coefficients))) # 85 unique covs in full


 ## -------------------------------------------------------------------


Mfull <- lm(formula = as.formula(paste('hs_correct_raven ~ . + ', substr(inputStr, 1, nchar(inputStr)-1))), data = fullData)

## LEVEL 2: Main effects and quadratics only
system.time({ 
  
  Mfwd2 <- step(object = M0, # base model
               scope = list(lower = M0, upper = Mfull),
               trace = 1, # trace prints out information
               direction = "forward")
  
})

length(unique(rownames(summary(Mfwd2)$coefficients))) # 123 unique covs in full


 ## -------------------------------------------------------------------

# holds all covars needing transformation, along with the specific transformation
sometrans
sometransInd <- which((colnames(fullData) %in% sometrans[, 1]))
notransInd <- which(!(colnames(fullData) %in% sometrans[, 1]))

fullDataModif <- fullData

# Transform columns in sometransInd, combine both into one big dataset, regress
for (i in 1:ncol(fullDataModif)) {
  if (i %in% sometransInd) {
    
    print(i)
    
    # Different control flow for each type of transformation
    sometransData <- sometrans[which(sometrans == colnames(fullData)[i]), ]
    if (sometransData[[2]] == 'quadratic') {
      
      print('inside quad')
       
      fullDataModif[, i] <- (fullDataModif[, i])^2
      colnames(fullDataModif)[i] <- paste(colnames(fullDataModif)[i], '^2', sep='')
      
    } else if (sometransData[[2]] == 'logarithmic') {
      
      print('inside log')
      
      fullDataModif[, i] <- logb(fullDataModif[, i] + 20)
      print('log DONE')
      colnames(fullDataModif)[i] <- paste('logb(', colnames(fullDataModif[i]), ')', sep='')
      print('naming DONE')
    }
     
  }
}

M0 <- lm(hs_correct_raven ~ 1, data = fullDataModif)
Mfull <- lm(formula = fullData$hs_correct_raven ~ ., data = fullDataModif)

## LEVEL 3: Main effects with their appropriate transformations
system.time({ 
  
  Mfwd3 <- step(object = M0, # base model
                scope = list(lower = M0, upper = Mfull),
                trace = 1, # trace prints out information
                direction = "forward")
  
})

length(unique(rownames(summary(Mfwd3)$coefficients))) 


 ## -------------------------------------------------------------------


## Need to add all main effects + selected interactions
## Since model with mains is better than model with transformations, we use mains only


inputStr2 <- paste(inters, ' + ', collapse='')
inputStr2 <- substr(inputStr2, 1, (nchar(inputStr2) - 4))

M0 <- lm(hs_correct_raven ~ 1, data = fullData)
Mfull <- lm(formula = as.formula(paste('hs_correct_raven ~ . + ', inputStr2, collapse='')), data = fullData)

## LEVEL 4: Main effects with their appropriate transformations + subjectively selected interaction terms
system.time({ 
  
  Mfwd4 <- step(object = M0, # base model
                scope = list(lower = M0, upper = Mfull),
                trace = 1, # trace prints out information
                direction = "forward")
  
})

length(unique(rownames(summary(Mfwd4)$coefficients))) 





##---------------------- Judging Quality of Model ----------------------- ## 


## Now we switch all selected models over to validation data, acquire
##  comparison statistics, perform kfold val for MSPE calculations

# number of cross-validation replications
k <- 7

validDataP <- validData[sample(nrow(validData)),] # permute rows
validDataP$index <- rep(1:k,each=nrow(validData)/k)

sometrans
sometransInd <- which((colnames(validDataP) %in% sometrans[, 1]))
notransInd <- which(!(colnames(validDataP) %in% sometrans[, 1]))

validDataPModif <- validDataP
# columns should be appropriately transformed
# Transform columns in sometransInd, combine both into one big dataset, regress
for (i in 1:ncol(validDataPModif)) {
  if (i %in% sometransInd) {
    
    # Different control flow for each type of transformation
    sometransData <- sometrans[which(sometrans == colnames(validDataPModif)[i]), ]
    if (sometransData[2] == 'quadratic') {
      
      validDataPModif[, i] <- (validDataPModif[, i])^2
      colnames(validDataPModif)[i] <- paste(colnames(validDataPModif)[i], '^2', sep='')
      
    } else if (sometransData[2] == 'logarithmic') {
      validDataPModif[, i] <- logb(validDataPModif[, i] + 20)
      colnames(validDataPModif)[i] <- paste('logb(', colnames(validDataPModif[i]), ')', sep='')
    }
    
  }
}

# storage space
mspe1 <- rep(NA, k) # mspe for M1
mspe2 <- rep(NA, k) # mspe for M2
mspe3 <- rep(NA, k) # mspe for M3
mspe4 <- rep(NA, k) # mspe for M4

aic1 <- rep(NA, k) # AIC for M1
aic2 <- rep(NA, k) # AIC for M2
aic3 <- rep(NA, k) # AIC for M3
aic4 <- rep(NA, k) # AIC for M4

adjr1 <- rep(NA, k) # Adj R2 for M1
adjr2 <- rep(NA, k) # Adj R2 for M2
adjr3 <- rep(NA, k) # Adj R2 for M3
adjr4 <- rep(NA, k) # Adj R2 for M4

r1 <- rep(NA, k) # R2 for M1
r2 <- rep(NA, k) # R2 for M2
r3 <- rep(NA, k) # R2 for M3
r4 <- rep(NA, k) # R2 for M4

system.time({
  for(ii in 1:k) {
    print(ii)
    
    train.ind <- which(validDataP$index!=ii) # training observations
    print(ii)
    # long-form cross-validation
    ## M1.cv <- lm(math ~ read + prog + race + ses + locus + read:prog + prog:ses,
    ##         data = hsbm, subset = train.ind)
    ## M2.cv <- lm(math ~ race + ses + sch + prog + locus + concept +
    #           mot + read + ses:sch + ses:concept + prog:read,
    #         data = hsbm, subset = train.ind)
    # using R functions
    M1.cv <- update(Mfwd1, subset = train.ind)
    print(ii)
    
    M2.cv <- update(Mfwd2, subset = train.ind)
    print(ii)
    
    M3.cv <- update(Mfwd3, subset = train.ind)
    print(ii)
    
    M4.cv <- update(Mfwd4, subset = train.ind)
    # cross-validation residuals
    # yi - yihat
    
    outcomes <- validDataP$hs_correct_raven[-train.ind]
    
    print(ii)
    
    # prediction with training data
    M1.res <- outcomes - predict(M1.cv, newdata = validDataP[-train.ind,]) 
    print(ii)

    M2.res <- outcomes - predict(M2.cv, newdata = validDataP[-train.ind,])
    print(ii)
    
    M3.res <- outcomes - predict(M3.cv, newdata = validDataPModif[-train.ind,]) 
    print(ii)
    
    M4.res <- outcomes - predict(M2.cv, newdata = validDataP[-train.ind,])
    # mspe for each model
    mspe1[ii] <- (mean(M1.res^2))
    mspe2[ii] <- (mean(M2.res^2))
    mspe3[ii] <- (mean(M3.res^2))
    mspe4[ii] <- mean(M4.res^2)
    
    aic1[ii] <- AIC(M1.cv)
    aic2[ii] <- AIC(M2.cv)
    aic3[ii] <- AIC(M3.cv)
    aic4[ii] <- AIC(M4.cv)
    
    adjr1[ii] <- summary(M1.cv)$adj.r.squared
    adjr2[ii] <- summary(M2.cv)$adj.r.squared
    adjr3[ii] <- summary(M3.cv)$adj.r.squared
    adjr4[ii] <- summary(M4.cv)$adj.r.squared
    
    r1[ii] <- summary(M1.cv)$r.squared
    r2[ii] <- summary(M2.cv)$r.squared
    r3[ii] <- summary(M3.cv)$r.squared
    r4[ii] <- summary(M4.cv)$r.squared
    
    
  }
})

# compare
par(mfrow = c(1,5))
cex <- 1
Mnames <- c('Mfwd1', 'Mfwd2', 'Mfwd3', 'Mfwd4')
boxplot(x = list(mspe1, mspe2, mspe3, mspe4), names = Mnames,
        main = "MSPE",
        #ylab = expression(sqrt(bar(SSE)[CV])),
        ylab = expression(MSPE),
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
boxplot(x = list(sqrt(mspe1), sqrt(mspe2), sqrt(mspe3), sqrt(mspe4)), names = Mnames,
        main = "Root MSPE",
        ylab = expression(sqrt(MSPE)),
        ## ylab = expression(SSE[CV]),
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
boxplot(x = list(r1, r2, r3, r4), names = Mnames,
        main = "R Squared",
        ylab = expression(R^2),
        ## ylab = expression(SSE[CV]),
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
boxplot(x = list(adjr1, adjr2, adjr3, adjr4), names = Mnames,
        main = "Adjusted R Squared",
        ylab = expression(AdjR^2),
        ## ylab = expression(SSE[CV]),
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
boxplot(x = list(aic1, aic2, aic3, aic4), names = Mnames,
        main = "AIC",
        ylab = expression(AIC),
        ## ylab = expression(SSE[CV]),
        col = c("yellow", "orange"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)


median(sqrt(mspe1))
median(sqrt(mspe2))
median(sqrt(mspe3))
median(sqrt(mspe4))

median(aic1)
median(aic2)
median(aic3)
median(aic4)

median(r1)
median(r2)
median(r3)
median(r4)

median(adjr1)
median(adjr2)
median(adjr3)
median(adjr4)



# From this, Model 2 is best model. Let's run on test to get values:

outcomes <- testData[, 'hs_correct_raven']
mspe <- mean((outcomes - predict(Mfwd2, newdata = testData))^2)
sqrt(mspe)
AIC(Mfwd2)
summary(Mfwd2)$r.squared
summary(Mfwd2)$adj.r.squared

















