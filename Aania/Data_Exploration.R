load("C:/Users/Onion/Downloads/exposome_NA.RData")

covariatesNA
codebook

phenotypeNA
exposomeNA
posts = codebook$variable_name[which(codebook$period == "Postnatal" & codebook$family == "Lifestyle")]
pregs = codebook$variable_name[which(codebook$period == "Pregnancy" & codebook$family == "Lifestyle")]
lifestyle = codebook$variable_name[which(codebook$family == "Lifestyle")]
lifestyle

n = length(posts)

for (i in 1:n) {
  posts[i] = toString(posts[i])
}

# creating a lifestyle codebook
exposomeNA[,posts]
lifestyle_mom <- intersect(lifestyle, colnames(exposomeNA))
lifestyle_mom
lifestyle_data <- exposomeNA[,lifestyle_mom]
lifestyle_codebook <- codebook[lifestyle_mom,]

# found that no variables in lifestyle exist in cov_data
cov_data <- intersect(lifestyle, colnames(covariatesNA))
cov_data
cov_codebook <- codebook[cov_data,]


# trying to fit all the lifestyle variables to raven score
lifestyle_data$hs_correct_raven <- phenotypeNA$hs_correct_raven
lifestyle_fit <- lm(data = lifestyle_data, formula = hs_correct_raven ~ .)

par(mar=c(4,4,4,4))
par(mfrow = c(2,2))

# investigating interval type data
# identifying data type and finding the range of different values
plot(lifestyle_data$h_cereal_preg_Ter)
plot(lifestyle_data$h_dairy_preg_Ter)
plot(lifestyle_data$h_meat_preg_Ter)
plot(lifestyle_data$hs_beverages_Ter)
plot(lifestyle_data$hs_org_food_Ter)
plot(lifestyle_data$h_fish_preg_Ter)

# creation of life_data codebook
life_codebook <- codebook[colnames(life_data),]

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

miss_raven <- which(is.na(life_data$hs_correct_raven))
# 10 missing values


# investigating raven score:
min(life_data$hs_correct_raven[-miss_raven]) # -27.7621
max(life_data$hs_correct_raven[-miss_raven]) # 146.7538
mean(life_data$hs_correct_raven[-miss_raven]) # 26.29357
sd(life_data$hs_correct_raven[-miss_raven]) # 6.441251
boxplot(life_data$hs_correct_raven[-miss_raven])

# generating a boxplot with different approaches to tackling the NAs:
# applying means to all NA values--
mean_raven <- life_data$hs_correct_raven
mean_raven[miss_raven] <- mean(life_data$hs_correct_raven[-miss_raven])

# checking distribution of values
boxplot(mean_raven)
sd(mean_raven) # 6.416429
mean(mean_raven) # 26.29357 same as old one


# applying random values between min and max of raven score
rand_raven <- life_data$hs_correct_raven
for (i in miss_raven) {
  rand_raven[i] <- runif(1, min(life_data$hs_correct_raven[-miss_raven]),
                         max(life_data$hs_correct_raven[-miss_raven]))
}

# checking distribution of values
boxplot(rand_raven) # similar, as expected, IQR seems the same too
sd(rand_raven) # 6.45971
mean(rand_raven) # 26.27621 same as old one
# the min and max values should be the same since the random values were
#   generated to not include the min and max values


# --------------------------investigating covariates-------------------------:
# ----------NUMERICAL----------:
# hs_mvsp_alt_none --
min(life_data$hs_mvpa_prd_alt_None) # -27.7621
max(life_data$hs_mvpa_prd_alt_None) # 146.7538
mean(life_data$hs_mvpa_prd_alt_None) # 37.87082
boxplot(life_data$hs_mvpa_prd_alt_None)
par(mar = c(3,3,3,3))

# piazza question 842

# hs_sd_week_none --
min(life_data$hs_sd_wk_None) # 3.142857 
max(life_data$hs_sd_wk_None) # 994.2857
mean(life_data$hs_sd_wk_None) # 235.8088
sd(life_data$hs_sd_wk_None) # 126.6873
boxplot(life_data$hs_sd_wk_None)

# hs_dif_hours_total_none -- 
min(life_data$hs_dif_hours_total_None) # 7.900729 
max(life_data$hs_dif_hours_total_None) # 12.85234
mean(life_data$hs_dif_hours_total_None) # 10.29592
sd(life_data$hs_dif_hours_total_None) # 0.7205823
boxplot(life_data$hs_dif_hours_total_None)


# ---------CATEGORICAL----------:
par(mfrow = c(4,4))

boxplot(life_data$hs_correct_raven, life_data$e3_alcpreg_yn_None)
life_codebook[2,]

# attempting to plot a pairs plot with categorical covariates
cat_life_data <- life_data[,-c(1,22,29,40)]
cols <- colnames(cat_life_data)
for (i in length(cols)) {
  cat_life_data[,i] <- as.factor(cat_life_data[,i])
}

# attempting to plot multiple boxplots with categorical variables
par(mfrow = c(3,3))

for (i in 2:10) {
  as.factor(new_data[,i])
  boxplot(new_data[,1]~new_data[,i], main=colnames(new_data)[i])
}

for (i in 11:19) {
  as.factor(new_data[,i])
  boxplot(new_data[,1]~new_data[,i], main=colnames(new_data)[i])
}

for (i in 20:28) {
  as.factor(new_data[,i])
  boxplot(new_data[,1]~new_data[,i], main=colnames(new_data)[i])
}

for (i in 29:37) {
  as.factor(new_data[,i])
  boxplot(new_data[,1]~new_data[,i], main=colnames(new_data)[i])
}



# investigating covariate relations to rule out interactions
# https://www.theanalysisfactor.com/interactions-main-effects-not-significant/#:~:text=There%20is%20really%20only%20one,%3A%20a%20cross%2Dover%20interaction.
# graphing the pairs of numerical and raven score variables
par(mar = c(2,2,2,2))
par(mfrow = c(1,1))
pairs(life_data[,c(1,22,29,40)], pch = 19)

# investigating covariate relations to rule out interactions
# graphing the pairs of categorical and raven score variables 
cat_life_data <- life_data[,-c(1,22,29,40)]
boxplot(life_data$hs_correct_raven, life_data[,1])


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

# number of coefficients with pvalue > 0.1 
# identifying coefficients with *enough* evidence supporting no association
length(pvals_inter[which(pvals_inter > 0.1)]) # 69

pvals_life_inter <- summary(Mlife_inter)$coefficients[,4]
pvals_inter[which(pvals_life_inter > 0.1)]
pvals_inter


# ---meat and lipids---
pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_meat_Ter*hs_total_lipids_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_total_meat_Ter, #x-axis variable
                 trace.factor = select$hs_total_lipids_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "meat",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "lipids")

# since the slopes of raven score for each lipid category changes for each meat 
#   category and the p_value is not much higher than 0.05 there is little evidence
#   against an interaction effect

# INTERACTION

# ---cereal and dairy---
unique(select$hs_dairy_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_cereal_Ter*hs_dairy_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_total_cereal_Ter, #x-axis variable
                 trace.factor = select$hs_dairy_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "cereal",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "dairy")

# though the pvalue for interaction effect is relatively high implying no
#   interaction effect, the graph shows interaction

# INTERACTION

# ---h_pavig and dog---
unique(select$h_pavig_t3_None) # 2

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_pet_dog_r2_None*h_pavig_t3_None)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_pavig_t3_None, #x-axis variable
                 trace.factor = select$hs_pet_dog_r2_None, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "phyiscal activity",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "pet_dog")

# the pvalue is 0.7+ and the graph generally has parallel lines which indicates
#   no significant interaction

# NO INTERACTION

# ---sweets and bakery---
unique(select$hs_total_sweets_Ter)
unique(select$hs_bakery_prod_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_sweets_Ter*hs_bakery_prod_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_total_sweets_Ter, #x-axis variable
                 trace.factor = select$hs_bakery_prod_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "sweets",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "bakery")

# p_value close 0.150 and graph shows differing raven score and bakery slopes
#   for each level of sweet

# INTERACTION

# ---hs_sd_wk_None and h_pavig---
unique(select$hs_sd_wk_None)
unique(select$h_pavig_t3_None)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_sd_wk_None*h_pavig_t3_None)

summary(pd_inter)

# p_value around 0.932 and interaction plot not possible

# NO INTERACTION

# ---h_meat_preg_Ter and h_veg_preg_Ter---
unique(select$h_meat_preg_Ter)
unique(select$h_veg_preg_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~h_meat_preg_Ter*h_veg_preg_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_meat_preg_Ter, #x-axis variable
                 trace.factor = select$h_veg_preg_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "meat",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "veg")

# the pvalue of the interaction effect is not too high but the graph
#   plot heavily proves an interaction effect

# INTERACTION

# beverages and caff drink
unique(select$hs_beverages_Ter)
unique(select$hs_caff_drink_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_beverages_Ter*hs_caff_drink_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_beverages_Ter, #x-axis variable
                 trace.factor = select$hs_caff_drink_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "raven score",
                 xlab = "beverages",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "caff")

# pvalue is low enough and plot implies an interaction

# INTERACTION



# hs_org_food_Ter and hs_proc_meat_Ter
unique(select$hs_org_food_Ter)
unique(select$hs_proc_meat_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_org_food_Ter*hs_proc_meat_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_org_food_Ter, #x-axis variable
                 trace.factor = select$hs_proc_meat_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "organic food",
                 xlab = "hs_proc_meat_Ter",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "processed meat")

# NO INTERACTIONS

# pamod and pavig

unique(select$h_pamod_t3_None)
unique(select$h_pavig_t3_None)

pd_inter <- aov(data = select, 
                hs_correct_raven~h_pamod_t3_None*h_pavig_t3_None)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_pamod_t3_None, #x-axis variable
                 trace.factor = select$h_pavig_t3_None, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "hs_correct",
                 xlab = "pavig",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "h_pavig_t3_None")

# INTERACTION

# ---mvpa_prd and pamod---

unique(select$h_pamod_t3_None)
unique(select$hs_mvpa_prd_alt_None)

pd_inter <- aov(data = select, 
                hs_correct_raven~h_pamod_t3_None*hs_mvpa_prd_alt_None)

summary(pd_inter)

par(mfrow=c(1,1))

library(ggplot2)
ggplot(select, aes(x=hs_mvpa_prd_alt_None, 
                    y=hs_correct_raven, color=h_pamod_t3_None)) + geom_point()

# NO INTERACTION

# ---readymade and fastfood---
unique(select$hs_readymade_Ter)
unique(select$h_fastfood_preg_Ter)

pd_inter <- aov(data = select, 
                hs_correct_raven~h_fastfood_preg_Ter*hs_readymade_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_readymade_Ter, #x-axis variable
                 trace.factor = select$h_fastfood_preg_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "hs_correct",
                 xlab = "readymade",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "fastfood")



