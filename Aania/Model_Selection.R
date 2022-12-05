# model selection for lifestyle data

# STEPWISE SELECTION

# WITHOUT INTERACTIONS
# full model with all lifestyle covariates and output hs_correct_raven
Mlife <- lm(data=life_data, formula = hs_correct_raven ~.)

# stepwise selection
# simplest model with no covariates and output hs_correct_raven
M0 <- lm(formula = hs_correct_raven ~ 1, data = life_data)

Mstep <- step(object = M0,
              scope = list(lower = M0, upper = Mlife), 
              direction = "both", trace = 1)
Mstep

# WITH INTERACTIONS
# full model with all lifestyle covariates and output hs_correct_raven WITH INTERACTIONS
#   between all possible covariates
Mlife_inter <- lm(data=life_data, formula = hs_correct_raven ~ (.)^2)
Mlife_inter

# stepwise selection
# simplest model with no covariates and output hs_correct_raven
M0 <- lm(formula = hs_correct_raven ~ 1, data = life_data)

Mstep_inter <- step(object = M0,
                    scope = list(lower = M0, upper = Mlife_inter), 
                    direction = "both", trace = 1)
Mstep_inter

# COMPARISON
# number of covariates in interaction model
length(coefficients(Mstep_inter)) # 138

# number of covariates in non-interaction model
length(coefficients(Mstep)) # 50
Mstep

# common covariates
cov_life <- rownames(summary(Mstep)$coefficients)
cov_life_inter <- rownames(summary(Mstep_inter)$coefficients)
length(intersect(cov_life, cov_life_inter))

plot(life_data$hs_readymade_Ter)
unique(life_data$hs_readymade_Ter)

cov_life_inter

miss <- which(is.na(y_train))
y_train[miss] <- mean(y_train[-c(196, 294, 429)])

# LASSO SELECTION

set.seed(999) ## for reproducibility

## load library for lasso/elastic net
library(glmnet)

# defining the response variable
y <- life_data$hs_correct_raven
X <- data.matrix(life_data[,-1])
X


## split into test and train
life_train <- life_data[sample(nrow(life_data)),] #jumbling up the order?
ntrain <- 500 # 500 training vals
train_id <- 1:ntrain
X_train <- X[train_id,] 
X_test <- X[-train_id,]
y_train <- y[train_id]
y_test <- y[-train_id]
y_train[miss] <- mean(y_train[-miss])


### LASSO
## fit models
M_lasso_life <- glmnet(x=X_train,y=y_train,alpha = 1)
y_train

## plot paths
plot(M_lasso,xvar = "lambda",label=TRUE)

## fit with crossval
cvfit_lasso <-  cv.glmnet(x=X_train,y=y_train,alpha = 1)

cvfit_lasso

## plot MSPEs by lambda
plot(cvfit_lasso)

## estimated betas for minimum lambda 
coef(cvfit_lasso, s = "lambda.min")## alternatively could use "lambda.1se"

## predictions
pred_lasso <- predict(cvfit_lasso,newx=X_test,  s="lambda.min")

## MSPE in test set
MSPE_lasso <- mean((pred_lasso-y_test)^2)



# investigating covariate relations to rule out interactions
par(mfrow = c(1,1))

pairs(life_data[,c(1,22,29,40)], pch = 19)

life_data[,c(1,23,30,40)]

plot(life_data$hs_correct_raven,life_data$hs_dif_hours_total_None, ylab="Raven Score", xlab = "dif hours total_None")

pairs(life_data, pch=19)



# checking full model with all possible two-way interactions and checking tests
#     of association to rule out whether the interaction terms were to be included
#     or not

summary(Mlife_inter)$coefficients[,4]
# for the interaction terms in the full lifestyle model, there are many Nans,
# implying the absence of data/lack of feasibility/reason to include or consider
# some interaction terms

# creating a list of pvalue terms for each coefficient
pvals_inter <- summary(Mstep_inter)$coefficients[,4]

# a list of all the coefficients whose pvalues were > 0.05 implying no association
#     with raven score
pvals_inter[which(pvals_inter > 0.1)]

# number of coefficients in reduced interaction model
length(summary(Mstep_inter)$coefficients[,4]) # 138

summary(Mstep_inter)$coefficients[,4]

# number of coefficients with pvalue > 0.1 
# identifying coefficients with *enough* evidence supporting no association
length(pvals_inter[which(pvals_inter > 0.1)]) # 69

pvals_life_inter <- summary(Mlife_inter)$coefficients[,4]
pvals_inter[which(pvals_life_inter > 0.1)]
pvals_inter


# running selection algorithm
install.packages("ggplot2")



install.packages("GGally")
library("GGally")
ggpairs(life_data[,-c(22,39,49)], upper=list(combo = "count"))



# ----------------------MODEL SELECTION EXPLORATION------------------------
install.packages("caret")
# splitting the train dataset into select and validation datasets
# using train_test function from Train_lass (srijan)
train_test(train)

# ----creation of covariates list for relevant models----

# full main effects lifestyle
lifestyle = rownames(codebook)[which(codebook$family == "Lifestyle")]
lifestyle

# ----LASSO-----
# fitting a main effects lifestyle model using lasso for variable screening
#   purposes
main_lasso_cov <- selection_lasso(select, lifestyle)
length(main_lasso_cov)
main_lasso_cov

# metrics (using OLS):
# creating a select df of screened variables and raven score
select_lasso_1 <- select[,main_lasso_cov]
select_lasso_1
select_lasso_1$hs_correct_raven <- select$hs_correct_raven

# fitting OLS model
M_lasso_1 <- lm(data = select_lasso_1, formula = hs_correct_raven ~.)
summary(M_lasso_1)

# variables with high pvalues
pvals_lasso_1 <- summary(M_lasso_1)$coefficients[,4]
pvals_lasso_1
high_pval_lasso_1 <- pvals_lasso_1[which(pvals_lasso_1 >= 0.7)]

coef(M_lasso_1)[which(pvals_lasso_1 >= 0.7)]
# all coefficient estimates generally < 1

par(mfrow = c(3,3))

# h_bfdur_Ter investigation
boxplot(select$hs_correct_raven~select$h_bfdur_Ter)
# levels generally look the same

# model diagnostics

# creating studentised residuals
stud_1 <- resid(M_lasso_1)/(sigma(M_lasso_1)*sqrt(1-hatvalues(M_lasso_1)))

# plotting residuals against fitted vals to check for equal variance assumption
plot(stud_1~fitted(M_lasso_1),
     xlab="Fitted Vals",
     ylab="Studentized Residuals",
     main="Residuals vs Fitted")

# plotting to check for normality
qqnorm(stud_1)
abline(0,1)

# looks fairlyyyyyyy normal
hist(stud_1,breaks=12,
     probability=TRUE,xlim=c(-4,4),
     xlab="Studentized Residuals",
     main="Distribution of Residuals")
grid <- seq(-3.5,3.5,by=0.05)
lines(x=grid,y=dnorm(grid),col="blue") # add N(0,1) pdf

# model goodness of fit
# adj R2:
# 0.3712

# R2:
summary(M_lasso_1)
# 0.4157

# AIC:
AIC(M_lasso_1)
# 5156.255

BIC(M_lasso_1)
# 5444.628

# standard error:
# 5.119

# fitting OLS without high pvalue covariates
high_pval_lasso_1_list <- c("h_bfdur_Ter", "h_fastfood_preg_Ter", "h_fish_preg_Ter",
                            "h_meat_preg_Ter", "hs_caff_drink_Ter","hs_diary_Ter",
                            "hs_fastfood_Ter","hs_pet_None")

high_pval_index_1 <- which(colnames(select_lasso_1) %in% high_pval_lasso_1_list == TRUE)
M_lasso_1.1 <- lm(data = select_lasso_1[,-high_pval_index_1], formula = hs_correct_raven ~.)
summary(M_lasso_1.1)

colnames(select_lasso_1)[high_pval_index_1]

# model goodness of fit
# adj R2:
# 0.3621

# R2:
summary(M_lasso_1.1)
# 0.3981

# AIC:
AIC(M_lasso_1.1)
# 5157.09

BIC(M_lasso_1.1)
# 5388.734

# standard error:
# 5.115


# ----BACKWARD----

selection_backward <- function (train, covariates) {
  library(olsrr)
  frmla <- as.formula(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), sep=""))
  model <- lm(frmla, data=train)
  ols_step_backward_p(model, prem=0.7)
  return (ols_step_backward_p(model, prem=0.7))
}

rem_back <- selection_backward(select, lifestyle)$remove
rem_back

which((colnames(select) %in% rem_back$removed) == TRUE)

lifestyle

setdiff(lifestyle, rem_back)

length(colnames(select[,setdiff(lifestyle, rem_back)]))

M_back_1 <- lm(data = cbind(select[,setdiff(lifestyle, rem_back)],hs_correct_raven = select$hs_correct_raven), 
               formula = hs_correct_raven ~.)
summary(M_back_1)

M_back_1_est <- summary(M_back_1)$coefficients[,4]
M_back_1_est[which(M_back_1_est>0.5)]

# model goodness of fit
# adj R2:
# 0.3987

# R2:
summary(M_back_1)
# 0.4434

# AIC:
AIC(M_back_1)
# 5121.787

BIC(M_back_1)
# 5424.343

# standard error:
# 5.006
# 

# df = 772

# Interesting how there's only one covariate in common between the high pvalue covariates in
#   backward and lasso high pvalue covariates

c("h_cereal_preg_Ter","h_fastfood_preg_Ter",
  "h_meat_preg_Ter", "h_pamod_t3_None",
  "hs_bakery_prod_Ter", "hs_fastfood_Ter")

boxplot(select$hs_correct_raven ~ select$h_cereal_preg_Ter)
boxplot(select$hs_correct_raven ~ select$h_fastfood_preg_Ter)
boxplot(select$hs_correct_raven ~ select$h_fish_preg_Ter)
boxplot(select$hs_correct_raven ~ select$h_legume_preg_Ter)
boxplot(select$hs_correct_raven ~ select$h_meat_preg_Ter)
boxplot(select$hs_correct_raven ~ select$h_pamod_t3_None)
boxplot(select$hs_correct_raven ~ select$hs_bakery_prod_Ter)
boxplot(select$hs_correct_raven ~ select$hs_dairy_Ter)
boxplot(select$hs_correct_raven ~ select$hs_fastfood_Ter)
boxplot(select$hs_correct_raven ~ select$hs_total_potatoes_Ter)

rem_back_1 <- c("h_cereal_preg_Ter","h_fastfood_preg_Ter",
                "h_meat_preg_Ter", "h_pamod_t3_None",
                "hs_bakery_prod_Ter", "hs_fastfood_Ter")

M_back_1.1 <- lm(data = cbind(select[,setdiff(lifestyle, rem_back_1)],hs_correct_raven = select$hs_correct_raven), 
               formula = hs_correct_raven ~.)
summary(M_back_1.1)

# model goodness of fit

# adj R2:
# 0.3815

# R2:
summary(M_back_1.1)
# 0.4231

# AIC:
AIC(M_back_1.1)
# 5139.697

# BIC:
BIC(M_back_1.1)
# 5413.888

# standard error:
# 5.076

# df = 778


# --------------LASSO WITH INTERACTIONS------------------

coef(M_lasso_1.1)
# considering interactions between:


# ---cat and dairy---
pd_inter <- aov(data = select, 
                hs_correct_raven~hs_pet_cat_r2_None*hs_dairy_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_pet_cat_r2_None, #x-axis variable
                 trace.factor = select$hs_dairy_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "cat",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Dairy")

# We see that raven score increases when going from no cat
#   to cat but for each level of dairy, the trend and 
#   slope of increase remains relatively the same
#   implying no/little interaction
# We do see also that the pvalue of no association is at ~0.344
#   which implies that the data sees no/little interaction

# NO INTERACTION




# -------------------LASSO 2.1------------------------
# fitting a model with lasso 1.1 covariates and above interactions
M_lasso_2.1 <- lm(data = select_lasso_1[,-high_pval_index_1], 
                  formula = hs_correct_raven ~. + hs_total_sweets_Ter:hs_bakery_prod_Ter
                  + hs_dairy_Ter:hs_total_cereal_Ter + hs_total_meat_Ter:hs_total_lipids_Ter)
summary(M_lasso_2.1)

# we see that the lipids and meat interaction effect remains prevalent
# sweets and bakery doesn't maintain much prevalence
# cereal and dairy don't maintain much prevalence either
# high pvalues: bakery (1 category), dairy, cereal, lipids
#   sweets, potatoes

# sigma hat: 5.153
# R2 : 0.4078
# adj R2 : 0.3627
# AIC:
AIC(M_lasso_2.1)
# 5167.489
BIC(M_lasso_2.1)
# 5455.862

# ------------------LASSO 2---------------------------
# fitting a model with lasso 1 covariates and above interactions
M_lasso_2 <- lm(data = select_lasso_1, 
                  formula = hs_correct_raven ~. 
                  + hs_total_sweets_Ter:hs_bakery_prod_Ter
                  + hs_dairy_Ter:hs_total_cereal_Ter 
                + hs_total_meat_Ter:hs_total_lipids_Ter)
summary(M_lasso_2)

# sigma hat: 5.119 on 763 df
# R2 : 0.4248
# adj R2 : 0.3712
# AIC:
AIC(M_lasso_2)
# 5167.255
BIC(M_lasso_2)
# 5512.357

# ------------------LASSO 3--------------------------
# running a selection algorithm with lasso and interactions

# creating a new df with interaction columns
select_lasso_3
select_lasso_3$sweetbakery <- select$hs_total_sweets_Ter:select$hs_bakery_prod_Ter
select_lasso_3$dairycereal <- select$hs_dairy_Ter:select$hs_total_cereal_Ter
select_lasso_3$meatlipids <- select$hs_total_meat_Ter:select$hs_total_lipids_Ter

# running selection algorithm on these covariates
cov_lasso_3 <- lifestyle
cov_lasso_3 <- append(lifestyle, c("sweetbakery", "dairycereal", "meatlipids"))


lasso_3_cov <- selection_lasso(select_lasso_3, cov_lasso_3)
length(lasso_3_cov)

lasso_3_cov
# only one interaction remained

# fitting these covariates using ols
select_lasso_3_df <- select_lasso_3[, lasso_3_cov]
select_lasso_3_df$hs_correct_raven = select$hs_correct_raven


M_lasso_3 <- lm(data = select_lasso_3_df, formula = hs_correct_raven ~.)
summary(M_lasso_3)
# getting NAs for some sweetbakery combination p values
# everything other than sweet bakery is still the same
# thus adding interactions has no effect as they either get
#   filtered out for multicollinearity or aren't feasible to
#   use as a predictor









