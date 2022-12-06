# Graphing and investigating select interactions

# ---meat and lipids---
# INTERACTION (1)
# running two-way anova to investigate interaction effect 
pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_meat_Ter*hs_total_lipids_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

# plotting an interaction plot ot visualise relationship
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

# CONSIDER INTERACTION


# ---cereal and dairy---
# INTERACTION (2)
pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_cereal_Ter*hs_dairy_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_cereal_preg_Ter, #x-axis variable
                 trace.factor = select$h_dairy_preg_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "raven score",
                 xlab = "cereal",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "dairy")

# though the pvalue for interaction effect is relatively high implying no
#   interaction effect, the graph shows interaction

# CONSIDER INTERACTION


# ---h_pavig and dog---

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


# ----hs_total_fish_Ter and h_folic_t1_None----

# INTERACTION (3)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_fish_Ter*h_folic_t1_None)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_total_fish_Ter, #x-axis variable
                 trace.factor = select$h_folic_t1_None, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "hs_correct",
                 xlab = "fish",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "folic acid")

# with a pvalue of around 0.0423, though the plot doesn't look too telling
#   an interaction should still be considered

# CONSIDER INTERACTION

# ---hs_sd_wk_None and h_pavig---
unique(select$hs_sd_wk_None)
unique(select$h_pavig_t3_None)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_sd_wk_None*h_pavig_t3_None)

summary(pd_inter)

library(ggplot2)
ggplot(select, aes(x=hs_sd_wk_None, 
                   y=hs_correct_raven, color=h_pavig_t3_None)) + geom_point()

# p_value around 0.932 and interaction plot shows no significant pattern

# NO INTERACTION

#------------------------------------------------------------------------------

# ---h_meat_preg_Ter and h_veg_preg_Ter---

# INTERACTION (4)

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

# the pvalue of the interaction effect is not too high and the graph
#   plot heavily implies an interaction effect

# CONSIDER INTERACTION

# ----beverages and caff drink----

# INTERACTION (5)
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

# CONSIDER INTERACTION


# ----hs_org_food_Ter and hs_proc_meat_Ter----
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

# plot doesn't show too much of an interaction effect and pvalue doesn't seem low
#   enough

# NO INTERACTIONS


# ----pamod and pavig----

# INTERACTION (6)

pd_inter <- aov(data = select, 
                hs_correct_raven~h_pamod_t3_None*h_pavig_t3_None)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_pamod_t3_None, #x-axis variable
                 trace.factor = select$h_pavig_t3_None, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "hs_correct",
                 xlab = "pavig",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "h_pavig_t3_None")

# pvalue seems high enough to suggest no interaction but the graph has differing
#   raven score-physical activity scores for some categories of pavig 
# CONSIDER INTERACTION


# ---mvpa_prd and pamod--

pd_inter <- aov(data = select, 
                hs_correct_raven~h_pamod_t3_None*hs_mvpa_prd_alt_None)

summary(pd_inter)

par(mfrow=c(1,1))

library(ggplot2)
ggplot(select, aes(x=hs_mvpa_prd_alt_None, 
                   y=hs_correct_raven, color=h_pamod_t3_None)) + geom_point()


# pvalue seems high enough to provide evidence against an interaction and graph 
#   contributes to that
# NO INTERACTION


# ---readymade and fastfood---
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

# pvalue seems high enough to provide evidence against an interaction
#   graph shows similar raven score-readymade slopes for each category of
#   fastfood

# NO INTERACTION


#---hs_dairy_Ter and hs_total_bread_Ter---

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_dairy_Ter*hs_total_bread_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_dairy_Ter, #x-axis variable
                 trace.factor = select$hs_total_bread_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "hs_correct",
                 xlab = "dairy",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "bread")

# pvalue too abnormally high to consider an interaction

# NO INTERACTION


# ----legume and cereal----

pd_inter <- aov(data = select, 
                hs_correct_raven~h_legume_preg_Ter*hs_total_cereal_Ter)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$h_legume_preg_Ter, #x-axis variable
                 trace.factor = select$hs_total_cereal_Ter, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "hs_correct",
                 xlab = "dairy",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "bread")

# The dairy-raven score slopes look quite similar for each category of bread
# pvalue seems high enough to imply no interaction

# NO INTERACTION


# ----hs_total_fish_Ter and hs_pet_cat_r2_None----

# INTERACTION (7)

pd_inter <- aov(data = select, 
                hs_correct_raven~hs_total_fish_Ter*hs_pet_cat_r2_None)

summary(pd_inter)

par(mfrow=c(1,1))

interaction.plot(x.factor = select$hs_total_fish_Ter, #x-axis variable
                 trace.factor = select$hs_pet_cat_r2_None, #variable for lines
                 response = select$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "hs_correct",
                 xlab = "fish",
                 col = c("pink", "blue", "green"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "cat")

# with a pvalue of around 0.00571 and a plot with slopes of opposite parity in
#   fish-raven score lines for each cat variable

# CONSIDER INTERACTION


# ---alcohol and sleep---

pd_inter <- aov(data = select, 
                hs_correct_raven~e3_alcpreg_yn_None*hs_mvpa_prd_alt_None)

summary(pd_inter)

par(mfrow=c(1,1))

library(ggplot2)
ggplot(select, aes(x=hs_mvpa_prd_alt_None, 
                   y=hs_correct_raven, color=e3_alcpreg_yn_None)) + geom_point()

select$e3_alcpreg_yn_None

# with a pvalue of ~0.9 and a plot with no relationship to be gathered, we ignore
#   this as an interaction

# NO INTERACTION