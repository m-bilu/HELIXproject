#Packages
library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)

data2<-load("exposome_NA.RData")
df<-load('/Users/kazirahman/Library/CloudStorage/OneDrive-UniversityofWaterloo/3A/Stat331/Final Project/exposome_NA.RData')

codebook<-codebook
exposome<-exposomeNA

lifestyles_df<-codebook[codebook$domain=='Lifestyles',]
postnatal_lifestyle<-lifestyles_df[lifestyles_df$period=='Postnatal',]
pregnancy_lifestyle<-lifestyles_df[lifestyles_df$period=='Pregnancy',]
df_pregnancy_lifestyle<-pregnancy_lifestyle

#pregnancy
#how many different subfamilies are there->12
losub<-c(pregnancy_lifestyle$subfamily)
length(losub)

#which subfamily do we have the most data on
df1<-data.frame(losub)
names(which.max(table(df1$losub)))
#DIET most number of data->8


#Some sort of relationship between the diet cov and the outcome
#df with just diet

df_diet<-df_pregnancy_lifestyle[!(df_pregnancy_lifestyle$subfamily=="Physical activity" |
                           df_pregnancy_lifestyle$subfamily=="Folic acid consumption"
                           |df_pregnancy_lifestyle$subfamily=="Prenatal Alcohol" 
                         ),]


lov_diet<-c("ID",df_diet$variable_name)#ignore this for now




#ExposomeNA and PhenotypeNA data
total_data <- merge(exposome,phenotypeNA,by="ID")
diet_data<-subset(total_data,select = c("ID",'h_cereal_preg_Ter',
                                        'h_dairy_preg_Ter','h_fastfood_preg_Ter',
                                        'h_fish_preg_Ter','h_fruit_preg_Ter',
                                        'h_legume_preg_Ter','h_meat_preg_Ter',
                                        'h_veg_preg_Ter',"hs_correct_raven"
                                          ))#this needs to be cleaned up
#diet_data ->contains possible cov of interest and outcome

#test model

M1<-lm(diet_data$hs_correct_raven~.,data = diet_data)


vif_values <- vif(M1)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2)

#postnatal and prenatal behavior of the mother and how it affects the 
#raven score

preg_data<-read.csv('preg_data.csv')
new_data<-data.frame(preg_data$h_pavig_t3_None)
v<-c(new_data$preg_data.h_pavig_t3_None)
v2<-as.factor(v)
str(new_data)
d2<-data.frame(v2)
d2$v2<-unclass(d2$v2)
replace_val<-c(d2$v2)

n<-replace(new_data, 1, replace_val)
#ignoring encoding the data

#splittting data into train and test

dt = sort(sample(nrow(preg_data), nrow(preg_data)*.7))
train<-preg_data[dt,]
test<-preg_data[-dt,]

#testing lasso on training set
library(glmnet)
y<-train$hs_correct_raven
#26.3042
x<-data.matrix(train[,c("e3_alcpreg_yn_None" , "h_cereal_preg_Ter" ,
                            "h_dairy_preg_Ter" ,   "h_fastfood_preg_Ter",
                             "h_fish_preg_Ter" ,    "h_folic_t1_None"   ,  
                            "h_fruit_preg_Ter"   , "h_legume_preg_Ter"  , 
                            "h_meat_preg_Ter" ,   
                            "h_pamod_t3_None" ,    "h_pavig_t3_None"  ,   
                            "h_veg_preg_Ter")])

y<-data.frame(y)
y[is.na(y)] <- 26.3042
y<-as.matrix(y)


cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda
#The lambda value that minimizes the test MSE turns out to be 0.04498289
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

library(ggplot2)
M<-lm()
interaction.plot(preg_data, pred = preg_data$hs_correct_raven, modx = preg_data$e3_alcpreg_yn_None)
