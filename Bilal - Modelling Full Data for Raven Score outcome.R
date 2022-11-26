################################
################################
#### PURPOSE:

## Performing step-wise and modified elastic net (seperately) to calculate
##    best variable selection for hs_correct_raven response variable. Comparing
##    both processes, gauging performance and deciding on final model
##    before moving on to final Beta estimation with OLS, 
##    model validation with leave-one-out cross val. Finally, deciding 
##    NEXT STEPS (WHAT WE DO AFTER GETTING THIS MODEL ?!)

## Loading in models, final cleaned data
## NOTE: for final calculations, use final cleaned data 
##    (with proper cleaning strategy)

load("C:/Users/mbila/Downloads/exposome_NA.RData")
fulldata <- read.csv('C:/Users/mbila/Documents/STAT 331 Final Project/full_clean_data_v1.csv')

# For now, no interaction terms, more on this later:
M0 <- lm(hs_correct_raven ~ ., data = data)


################################ STEPWISE:
system.time({
  Mstep <- step(object = M0,
                scope = list(lower = M0, upper = Mfull), 
                direction = "both", trace = 1)
})


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



