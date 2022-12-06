
# ----------------------LIFESTYLE MODEL SELECTION EXPLORATION------------------------

# ----LASSO-----
# fitting a main effects lifestyle model using lasso for variable screening
#   purposes

# creation of covariates list
lifestyle = rownames(codebook)[which(codebook$family == "Lifestyle")]

# running lasso selection to reduce list of covariates
main_lasso_cov <- selection_lasso(select, lifestyle) # reduced list of covariates

# metrics (using OLS):
# creating a select df of reduced variables and raven score
select_lasso_1 <- select[,main_lasso_cov]
select_lasso_1$hs_correct_raven <- select$hs_correct_raven

# fitting OLS model
M_lasso_1 <- lm(data = select_lasso_1, formula = hs_correct_raven ~.)

# investigating variables with 'high' pvalues
# 'high' was defined as > 0.7 for the sake of removing pvalues approaching 1
pvals_lasso_1 <- summary(M_lasso_1)$coefficients[,4]
high_pval_lasso_1 <- pvals_lasso_1[which(pvals_lasso_1 >= 0.7)]

# model goodness of fit metrics 
# adj R2:
# 0.3659

# R2:
summary(M_lasso_1)
# 0.4024

# AIC:
AIC(M_lasso_1)
# 5161.16

BIC(M_lasso_1)
# 5397.531

# standard error:
# 5.165


# SECOND LASSO MODEL
# Fitting a second model excluding all covariates in the previous model with
#   high pvalue (high meaing >0.7)

# fitting OLS without high pvalue covariates
high_pval_lasso_1 <- pvals_lasso_1[which(pvals_lasso_1 >= 0.7)]
high_pval_lasso_1_list <- c("h_fish_preg_Ter",
                            "h_fruit_preg_Ter", "hs_break_cer_Ter",
                            "hs_total_lipids_Ter")

high_pval_index_1 <- which(colnames(select_lasso_1) %in% high_pval_lasso_1_list == TRUE)



M_lasso_1.1 <- lm(data = select_lasso_1[,-high_pval_index_1], formula = hs_correct_raven ~.)
summary(M_lasso_1.1)


# model goodness of fit
# adj R2:
# 0.3665

# R2:
summary(M_lasso_1.1)
# 0.3969

# AIC:
AIC(M_lasso_1.1)
# 5152.786

BIC(M_lasso_1.1)
# 5351.338

# standard error:
# 5.163

# ------------------LASSO 2---------------------------
# fitting a model with lasso 1 covariates and some select interactions
M_lasso_2 <- lm(data = select_lasso_1, 
                formula = hs_correct_raven ~. 
                + h_dairy_preg_Ter:hs_total_cereal_Ter 
                + hs_total_meat_Ter:hs_total_lipids_Ter
                + hs_total_fish_Ter:hs_pet_cat_r2_None)
summary(M_lasso_2)

# sigma hat: 5.112
# R2 : 0.4221
# adj R2 : 0.3789
# AIC:
AIC(M_lasso_2)
# 5153.116
BIC(M_lasso_2)
# 5436.762

# -------------------LASSO 2.1------------------------
# fitting a model with lasso 1.1 covariates and interactions
#   whose covariates exist in the reduced df
M_lasso_2.1 <- lm(data = select_lasso_1[,-high_pval_index_1], 
                  formula = hs_correct_raven ~. 
                  + hs_total_fish_Ter:hs_pet_cat_r2_None
                  + h_dairy_preg_Ter:hs_total_cereal_Ter 
          )
summary(M_lasso_2.1)

# we see that the cereal and dairy interaction doesn't maintain
#   much prevalence across all categories in terms of pvalues
# cat and fish interaction doesn't maintain much prevalence

# sigma hat: 5.122
# R2 : 0.4108
# adj R2 : 0.3764
# AIC:
AIC(M_lasso_2.1)
# 5145.344
BIC(M_lasso_2.1)
# 5372.261

# ------------------LASSO 3--------------------------
# running a selection algorithm with lasso and interactions 1-3

# creating a new df with interaction columns
select_lasso_3 <- select
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

# it seems that all interactions except hs_total_sweets_Ter:hs_bakery_prod_Ter
#   were removed by the lasso algorithm
# getting NAs for some sweetbakery combination p values
# everything other than sweet bakery is still the same
# thus adding interactions has no effect as they either get
#   filtered out for multicollinearity or aren't feasible to
#   use as a predictor


# sigma hat: 5.151
# R2 : 0.4102
# adj R2 : 0.3694
# AIC:
AIC(M_lasso_3)
# 5162.196
BIC(M_lasso_3)
# 5426.933

#----------------Lasso (final)----------------------

# Running a lasso selection algorithm with interactions 4 to 8

# creating a data set containing interaction columns pertaining to interactions
#   4 to 8
lasso_final_data <- select
lasso_final_data$pamodpavig <- select$h_pamod_t3_None:select$h_pavig_t3_None
lasso_final_data$bevcaff <- select$hs_beverages_Ter:select$hs_caff_drink_Ter
lasso_final_data$meatveg <- select$h_meat_preg_Ter:select$h_veg_preg_Ter
lasso_final_data$fishcat <- as.factor(as.character(select$hs_total_fish_Ter)):as.factor(as.character(select$hs_pet_cat_r2_None))
lasso_final_data$folicfish <- as.factor(as.character(select$h_folic_t1_None)):as.factor(as.character(select$hs_pet_cat_r2_None))

# vector of covariates of interest
cov_final_lasso <- lifestyle
cov_final_lasso <- append(lifestyle, c("pamodpavig", "bevcaff", "meatveg", 
                                       "fishcat", "folicfish"))

# reduced vector of covariates
red_cov_lasso <- selection_lasso(lasso_final_data, cov_final_lasso)

# fitting these covariates using ols
select_lasso_final_df <- lasso_final_data[, red_cov_lasso]
select_lasso_final_df$hs_correct_raven = select$hs_correct_raven

M_lasso_final <- lm(data = select_lasso_final_df, formula = hs_correct_raven ~.)
summary(M_lasso_final)

# we see that many interaction terms in our final model have NA values since the 
#  data isn't enough to facilitate the creation of estimates and the rest of the
#  interaction terms have quite high (>0.70) pvalues


# sigma hat: 5.24
# R2 : 0.3958
# adj R2 : 0.3473
# AIC:
AIC(M_lasso_final)
# 5198.286
BIC(M_lasso_final)
# 5500.842

# COMPARING ALL ABOVE MODELS BY MSPE

# MODEL COMPARISONS

valid_final_data <- validation

valid_final_data$pamodpavig <- validation$h_pamod_t3_None:validation$h_pavig_t3_None
valid_final_data$bevcaff <- validation$hs_beverages_Ter:validation$hs_caff_drink_Ter
valid_final_data$meatveg <- validation$h_meat_preg_Ter:validation$h_veg_preg_Ter
valid_final_data$fishcat <- as.factor(as.character(validation$hs_total_fish_Ter)):as.factor(as.character(validation$hs_pet_cat_r2_None))
valid_final_data$folicfish <- as.factor(as.character(validation$h_folic_t1_None)):as.factor(as.character(validation$hs_pet_cat_r2_None))
valid_final_data$sweetbakery <- validation$hs_total_sweets_Ter:validation$hs_bakery_prod_Ter

# comparisons of MSPE
ntot <- nrow(valid_final_data) # total number of observations

# number of cross-validation replications
Kfolds <- 7

val_1 <- valid_final_data[sample(ntot),] # permute rows
val_1$index <- rep(1:Kfolds,each=ntot/Kfolds)


# storage space
mspe1 <- rep(NA, Kfolds) # mspe for M_lasso_1, reduced main effects
mspe2 <- rep(NA, Kfolds) # mspe for M_lasso_1.1, low pvalue reduced main effects
mspe3 <- rep(NA, Kfolds) # mspe for M_lasso_final
mspe4 <- rep(NA, Kfolds) # mspe for M_lasso_3
mspe5 <- rep(NA, Kfolds) # mspe for M_lasso_2
mspe6 <- rep(NA, Kfolds) # mspe for M_lasso_2.1

system.time({
  for(ii in 1:Kfolds) {
    
    train.ind <- which(val_1$index!=ii) # training observations
    
    
    # using R functions
    M_lasso_1.cv <- update(M_lasso_1, subset = train.ind)
    M_lasso_1.1.cv <- update(M_lasso_1.1, subset = train.ind)
    M_lasso_final.cv <- update(M_lasso_final, subset = train.ind)
    M_lasso_3.cv <- update(M_lasso_3, subset = train.ind)
    M_lasso_2.cv <- update(M_lasso_2, subset = train.ind)
    M_lasso_2.1.cv <- update(M_lasso_2.1, subset = train.ind)
    # cross-validation residuals
    M_lasso_1.res <- val_1$hs_correct_raven[-train.ind] - # test observations
      predict(M_lasso_1.cv, newdata = val_1[-train.ind,]) # prediction with training data
    M_lasso_1.1.res <- val_1$hs_correct_raven[-train.ind] -predict(M_lasso_1.1.cv, newdata = val_1[-train.ind,])
    M_lasso_final.res <- val_1$hs_correct_raven[-train.ind] - # test observations
      predict(M_lasso_final.cv, newdata = val_1[-train.ind,]) # prediction with training data
    M_lasso_3.res <- val_1$hs_correct_raven[-train.ind] - # test observations
      predict(M_lasso_3.cv, newdata = val_1[-train.ind,]) # prediction with training data
    M_lasso_2.res <- val_1$hs_correct_raven[-train.ind] - # test observations
      predict(M_lasso_2.cv, newdata = val_1[-train.ind,]) # prediction with training data
    M_lasso_2.1.res <- val_1$hs_correct_raven[-train.ind] - # test observations
      predict(M_lasso_2.1.cv, newdata = val_1[-train.ind,]) # prediction with training data
    
    # mspe for each model
    mspe1[ii] <- mean(M_lasso_1.res^2)
    mspe2[ii] <- mean(M_lasso_1.1.res^2)
    mspe3[ii] <- mean(M_lasso_final.res^2)
    mspe4[ii] <- mean(M_lasso_3.res^2)
    mspe5[ii] <- mean(M_lasso_2.res^2)
    mspe6[ii] <- mean(M_lasso_2.1.res^2)
  }
})

# compare
par(mfrow = c(1,1))

cex <- 1
boxplot(x = list(lasso1 = mspe1, lasso1.1 = mspe2, lasso2 = mspe5,
                 lasso2.1 = mspe6,
                 lassofinal=mspe3,
                 lasso3 = mspe4)
        ,
        main = "MSPE",
        #ylab = expression(sqrt(bar(SSE)[CV])),
        ylab = expression(MSPE),
        col = c("yellow", "orange", "blue", "green", "red", "purple"),
        cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)
boxplot(x = list(lasso1 = sqrt(mspe1), lasso1.1 = sqrt(mspe2),
                 lasso2 = sqrt(mspe5), lasso2.1 = sqrt(mspe6),
                 lasso3 = sqrt(mspe4),
                 lassofinal = sqrt(mspe3) 
),
main = "Root MSPE",
ylab = expression(sqrt(MSPE)),
## ylab = expression(SSE[CV]),
col = c("red", "orange", "yellow", "brown", "grey", "pink"),
cex = cex, cex.lab = cex, cex.axis = cex, cex.main = cex)

# avgs of mspe and rmspe for all models

# lasso 1: main effects selection done by lasso
mean(mspe1)
mean(sqrt(mspe1))

# lasso 1.1: rerunning the previous model after removing high pvalue covs
mean(mspe2)
mean(sqrt(mspe2))

# lassofinal: running lasso model selection with all main effects and new
#   selection interactions
mean(mspe3)
mean(sqrt(mspe3))

# lasso 3: running lasso model selections with all main effects and some 
#   select interactions (as found on interaction section of code)
mean(mspe4)
mean(sqrt(mspe4))

# lasso 2: rerunning lasso 1 with carefully selected interaction terms
mean(mspe5)
mean(sqrt(mspe5))

# lasso 2.1: rerunning lasso 1.1 with carefully selected interaction terms
mean(mspe6) # around the same as the lasso 1.1
mean(sqrt(mspe6))



