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

# Defining worst/best model for step function
M0 <- lm(hs_correct_raven ~ 1, data = fulldata)


################################ FORWARD Selection:
## Using fwd so we dont need to mention upper model
## upper model would include all main effects and interactions,
## that is 60000 covariates in one model.

## Trick for including interaction terms in step function without 
##    including them in lm model (which would take a while):
##    https://stackoverflow.com/questions/22418116/adding-interaction-terms-to-step-aic-in-r
inputStr <- paste(' I(',colnames(fulldata)[colnames(fulldata)!='hs_correct_raven'],"^2) +", collapse='') 

system.time({
  Mfwd <- step(object = M0, # base model
               scope = . ~ .^2 + 
                 substr(inputStr, 1, nchar(inputStr)-1),
               trace = 1, # trace prints out information
               direction = "forward" )
  
  substr(., 1, nchar(.)-1)
})

substr(inputStr, 1, nchar(inputStr)-1)






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


