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


## HELPER FUNCTION:
train_test <- function(data) {
  # Input : data you want to split
  
  
  # Returns:
  #data split into training and testing
  # train_and_test <- train_test(the_complete_dataset)
  # train <- train_and_test$train
  # test <- train_and_test$test
  library(caret)
  set.seed(331)
  random_sample <- createDataPartition(data$hs_correct_raven, p = 0.8, 
                                       list = FALSE)
  training_data  <- data[random_sample, ]
  testing_data <- data[-random_sample, ]
  list(train = training_data, test = testing_data)
  return(training_data , testing_data )
}

library(ggplot2)
load("C:/Users/mbila/Downloads/exposome_NA.RData")
fulldata <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/Data/full_clean_data_v2.csv')


## SPLITTING DATA:
split1 <- train_test(fulldata)

# FINISH AFTER KAZI SPLITS DATA
# CONTINUE WITH CODE



 
 ## -------------- Subjective: Covariate Transformation --------------- ##



# First, lets analyze the relationship in between outcome and covariates,
#   lets find covariates that need transformation
#   IE they have quadratic, logarithmic, etc. relationship with outcome

# Graphs for interaction terms:
# Aania Looking through lifestyle covs, remove then graph
lifeStyle <- as.character(codebook[codebook$family=='Lifestyle', 'variable_name'])
lifeStyle <- c(lifeStyle, 'hs_correct_raven')
lifeStyleInd <- which(colnames(fulldata) %in% lifeStyle)
dataNoLife <- fulldata[, -lifeStyleInd]

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
      
    boxplot(fulldata$hs_correct_raven~dataNoLife[, i], 
              ylab='Raven Score', 
              xlab=colnames(dataNoLife)[i], 
              main='Boxplot', ann=TRUE)
    
    # if continuous, draw scatterplot
  } else {
    
    plot(fulldata$hs_correct_raven~dataNoLife[, i], 
           ylab='Raven Score', 
           xlab=colnames(dataNoLife)[i], 
           main='Plot', ann=TRUE)
  }
  
  if (i %% 3) {
    dev.off()
    pdf(paste('C:/Users/mbila/Documents/STAT 331 Final Project/Bilal/outcomeGraphs/graph', i, '.pdf'))
  }
}

dev.off()

## PDF Stored
## List of subjective covs and transformation ideas

subj <- data.frame('e3_yearbir_None', 'quadratic')
names(subj) <- c('Covariate', 'Relationship')
add <- data.frame('h_parity_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_parity_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_child_age_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_c_height_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_c_weight_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_pm25_yr_hs_h_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_accesspoints300_preg_Log', 'wierd')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_fdensity300_preg_Log', 'wierd')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_landuseshan300_h_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_landuseshan300_s_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_TEX_Log', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_cs_c_Log2', 'quadratic') # + 5 to remove 0 values for log
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_humidity_preg_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_KIDMED_None', 'quadratic') # COULD BE LOG
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('h_temperature_preg_None', 'quadratic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_hum_mt_hs_h_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_tm_mt_hs_h_None', 'logarithmic') # + 10
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_hum_wk_hs_h_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_tm_dy_hs_h_None', 'logarithmic')# + 20
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_tm_wk_hs_h_None', 'logarithmic')# + 10
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_pcb118_madj_Log2', 'logarithmic')# + 5
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_pcb170_madj_Log2', 'logarithmic') # + 5
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)
add <- data.frame('hs_hm_pers_None', 'logarithmic')
names(add) <- c('Covariate', 'Relationship')
subj <- rbind(subj, add)

# subj now holds the names and relationships of all covs that require transformation 





  ## -------------------- Subjective: Cov Interaction --------------------- ##





#### Let's print cov descriptions
codebook[which(codebook$family != 'Lifestyle'), 'description']
# blood content during pregnancy may be connected
inter1 <- dataNoLife[, 1:4]
# blood content during hs test may be connected
inter2 <- dataNoLife[, c(5:16, 26)]
# transport mode lines, mode stops, building/connectivity density
inter3 <- dataNoLife[, c(17:22, 24)]
# 



  ## ------------------------- FORWARD Selection -------------------------- ##


## Using fwd so we dont need to mention upper model
## upper model would include all main effects and interactions,
## that is 56882 covariates in one model.

## IMPORTANT NOTE:
## Quadratic terms are only relevant for continuous covariates
fulldata_cont <- fulldata[, which(sapply(fulldata, is.numeric))]
inputStr <- paste(' I(', colnames(fulldata_cont)[colnames(fulldata_cont)!='hs_correct_raven'] , "^2) +", collapse='') 

# Defining worst/best model for step function
M0 <- lm(hs_correct_raven ~ 1, data = fulldata)
Mfull <- lm(hs_correct_raven ~ ., data = fulldata)

## LEVEL 1: Main effects and quadratics
system.time({ 
  
  Mfwd1 <- step(object = M0, # base model
               scope = list(lower = M0, upper = Mfull),
               trace = 1, # trace prints out information
               direction = "forward")
  
})

length(unique(rownames(summary(Mfwd1)$coefficients))) # 85 unique covs in full

Mfull <- lm(formula = as.formula(paste('hs_correct_raven ~ . + ', substr(inputStr, 1, nchar(inputStr)-1))), data = fulldata)

## LEVEL 2: Main effects and quadratics
system.time({ 
  
  Mfwd2 <- step(object = M0, # base model
               scope = list(lower = M0, upper = Mfull),
               trace = 1, # trace prints out information
               direction = "forward")
  
})

length(unique(rownames(summary(Mfwd2)$coefficients))) # 99, 85 + 14 unique covs in full





##---------------------- Judging Quality of Model ----------------------- ## 



# Which covariates are not in Mfwd2 but are in Mfwd1
setdiff(rownames(summary(Mfwd1)$coefficients), rownames(summary(Mfwd2)$coefficients))

# Vice Versa - Only quadratic terms
setdiff(rownames(summary(Mfwd2)$coefficients), rownames(summary(Mfwd1)$coefficients))

## If this current model can predict well, we good, no need to add interactions
summary(Mfwd1)$adj.r.squared
summary(Mfwd2)$adj.r.squared

AIC(Mfwd1)
AIC(Mfwd2)

BIC(Mfwd1)
BIC(Mfwd2)

# MSPE  # COPY SRIJAN'S CODE




























################################ NOTE: INTERACTION TERMS

## QUESTION: Do I include interaction terms in the model?
# We have to measure the importance of an interaction term x before including
#   in model.

# How? To measure 'importance' of covariate in model, measure 2 things
#   Impact on outcome y, multicollinearity with rest of models
#   Multicollinearity is already measured 
#   Discussion for later
#   MAYBE we can discover that interaction terms of degree higher than some n
#   have an estimate appraoching 0 (becoming useless), or SMTH LIKE THAT
#   With that proof, we can firmly conclude that we only need interaction
#   terms up until n degree

# OTHER IDEA: Draw scatterplots of y and each cov, each cov with each cov
#   to measure linear relationship

## IDEA: After model selection, reflect on these three:
#   - Covariates that got selected by both processes, but make no sense irl
#   - Covariates that got selected by none, but should affect outcome irl
#   - Covariates that only got selected by one process, not the other

## ANSWER ON INTERACTION TERMS:
# https://www.reddit.com/r/statistics/comments/mlclj5/q_is_a_fourway_interaction_ever_a_good_idea/
#   - 3-way, 4-way interaction terms are not interprable
#   - Also overfitting (including 4way -> including 4 3ways, 6 2ways, 4 main effects)
#   - We can use either argument, but using overfitting argument would 
#   - require some stat proof backing.
#   - Overfitting can be ignrd by backward-stepwise, but severe p-flation

## QUESTION OF GROUP:
# What are the final conclusions of our q? Inference, Prediction?
# Asking since our model must change to prioritize either
# Deciding we dont need more covariates bc CI/PI is too variable
# MODEL SELECTION ON SUBSET OF DATA? Doing so would make CI's accurate
#   But is that really a priority for our q?

# Check in with on Aania list of summary statistics

# Check in with Bilal, Aania on validation strategy
#   (Training, Val, Test)

