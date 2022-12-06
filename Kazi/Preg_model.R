library(glmnet)
library(Metrics)
setwd("/Users/kazirahman/Library/CloudStorage/OneDrive-UniversityofWaterloo/3A/Stat331/Final Project")
#load in data
##loading new test and train data for everyone

test_data<-read.csv("Data/test_final.csv")
data<-read.csv('Data/train_final.csv')
data<-subset(data, select = -c(X) )
#Checking for Nans
nan_count <-sapply(data, function(data) sum(length(which(is.na(data)))))
nan_count <- data.frame(nan_count)
nan_count
preg<-read.csv('Data/preg_data.csv')

loc_preg<-c(names(preg))
#data for pregnancy variables under lifestyle family
preg_data<-data[loc_preg]
#splitting data into train and valid
train_val <- function(data) {
  library(caret)
  set.seed(331)
  random_sample <- createDataPartition(data$hs_correct_raven, p = 0.5, 
                                       list = FALSE)
  training_data  <- data[random_sample, ]
  validation_data <- data[-random_sample, ]
  list(train = training_data, validation = validation_data)
}
train_data<-train_val(preg_data)$train
validation_data<-train_val(preg_data)$validation

#we use train_data to do feature selection
set.seed(331)
#Lasso
x<-data.matrix(train_data[,c("e3_alcpreg_yn_None" , "h_cereal_preg_Ter" ,
                       "h_dairy_preg_Ter" ,   "h_fastfood_preg_Ter",
                       "h_fish_preg_Ter" ,    "h_folic_t1_None"   ,  
                       "h_fruit_preg_Ter"   , "h_legume_preg_Ter"  , 
                       "h_meat_preg_Ter" ,   
                       "h_pamod_t3_None" ,    "h_pavig_t3_None"  ,   
                       "h_veg_preg_Ter")])

cv_model <- cv.glmnet(x, train_data$hs_correct_raven, alpha = 1)
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

best_model <- glmnet(x, train_data$hs_correct_raven, alpha = 1, lambda = best_lambda)
coef(best_model)

test_cov_ind<-which(coef(best_model)==0)
excluding_var<-c()
for (i in test_cov_ind){
  excluding_var <- c(excluding_var, names(train_data[i]))
}
print("cov that we may exclude")
print(excluding_var)

##Models
#fitting the full model
lmfull <- lm(validation_data$hs_correct_raven ~ ., data=validation_data)
#fitting to a constant
lm<- lm(validation_data$hs_correct_raven ~ 1, data=validation_data)
#Lasso
lm_lasso<-lm(train_data$hs_correct_raven~e3_alcpreg_yn_None+h_cereal_preg_Ter+
             h_dairy_preg_Ter +h_fastfood_preg_Ter+
             h_fish_preg_Ter+ h_folic_t1_None+ 
             h_fruit_preg_Ter+h_legume_preg_Ter+ 
             h_meat_preg_Ter+
             h_pamod_t3_None,data = train_data)
#step-wise

system.time({
  Mstep <- step(object = lm,
                scope = list(lower = lm, upper = lmfull), 
                direction = "both", trace = 1)
})
lm_stepwise<-lm(validation_data$hs_correct_raven ~ h_cereal_preg_Ter + h_dairy_preg_Ter + 
                  h_folic_t1_None + h_fruit_preg_Ter + h_legume_preg_Ter,data = validation_data)

#interactions
#lasso coeff
lm_lasso_inter<-lm(validation_data$hs_correct_raven~(e3_alcpreg_yn_None+h_cereal_preg_Ter+
               h_dairy_preg_Ter +h_fastfood_preg_Ter+
               h_fish_preg_Ter+ h_folic_t1_None+ 
               h_fruit_preg_Ter+h_legume_preg_Ter+ 
               h_meat_preg_Ter+
               h_pamod_t3_None+h_pavig_t3_None)^2,data = validation_data)
#stepwise
lm_stepwise_inter<-lm(validation_data$hs_correct_raven ~ (h_cereal_preg_Ter + h_fastfood_preg_Ter + 
                  h_legume_preg_Ter + h_folic_t1_None)^2,data = validation_data)

#comparing models
#figure out between lasso and step-wise
Rsquared_lasso<-summary(lm_lasso)$adj.r.squared
Rsquared_stepwise<-summary(lm_stepwise)$adj.r.squared
test_data<-subset(test_data, select = -c(X))
test_data<-test_data[loc_preg]



pred_lasso <- data.frame(actual=c(test_data$hs_correct_raven),
                   predicted=c(predict(lm_lasso, test_data, type="response")))


RMSE_lasso<-rmse(pred_lasso$actual, pred_lasso$predicted)



pred_step_wise <- data.frame(actual=c(test_data$hs_correct_raven),
                         predicted=c(predict(lm_stepwise, test_data, type="response")))


RMSE_stepwise<-rmse(pred_step_wise$actual, pred_step_wise$predicted)

################# Interactions ##############
#e3_alcpreg_yn_None+h_cereal_preg_Ter+
#  h_dairy_preg_Ter +h_fastfood_preg_Ter+
#  h_fish_preg_Ter+ h_folic_t1_None+ 
#  h_fruit_preg_Ter+h_legume_preg_Ter+ 
#  h_meat_preg_Ter+
#  h_pamod_t3_None+h_pavig_t3_None,data

lm_lasso_inter<-lm(validation_data$hs_correct_raven~(e3_alcpreg_yn_None+h_cereal_preg_Ter+
                                                       h_dairy_preg_Ter +h_fastfood_preg_Ter+
                                                       h_fish_preg_Ter+ h_folic_t1_None+ 
                                                       h_fruit_preg_Ter+h_legume_preg_Ter+ 
                                                       h_meat_preg_Ter+
                                                       h_pamod_t3_None+h_pavig_t3_None)^2,data = validation_data)


loc<-c("e3_alcpreg_yn_None"             ,               "h_cereal_preg_Ter",                                 
       "h_dairy_preg_Ter"             ,               "h_dairy_preg_Ter"  ,                              
       "h_fastfood_preg_Ter"   ,                  "h_fastfood_preg_Ter" ,                            
        "h_fish_preg_Ter"      ,                    "h_fish_preg_Ter" ,                                 
       "h_folic_t1_None"                ,                "h_meat_preg_Ter",                                   
       "h_pavig_t3_None"             ,                  "h_pavig_t3_None")

#trying to make a new dataset to deal with interactions
dd<-model.matrix(~(e3_alcpreg_yn_None+h_cereal_preg_Ter+
                     h_dairy_preg_Ter +h_fastfood_preg_Ter+
                     h_fish_preg_Ter+ h_folic_t1_None+ 
                     h_fruit_preg_Ter+h_legume_preg_Ter+ 
                     h_meat_preg_Ter+
                     h_pamod_t3_None+h_pavig_t3_None),preg_data)



dd<-data.frame(dd)
dd$aclpregcereal9<-as.factor(as.character(dd$e3_alcpreg_yn_None*dd$h_cereal_preg_Ter.9.27.3.))


listofinteractions<-c("e3_alcpreg_yn_None:h_cereal_preg_Ter(9,27.3]"       ,        "e3_alcpreg_yn_None:h_dairy_preg_Ter(17.1,27.1]" ,           
                       "e3_alcpreg_yn_None:h_dairy_preg_Ter(27.1,Inf]"      ,        "e3_alcpreg_yn_None:h_fish_preg_Ter(1.9,4.1]",               
                       "e3_alcpreg_yn_None:h_fish_preg_Ter(4.1,Inf]"         ,       "e3_alcpreg_yn_None:h_folic_t1_None" ,                       
                       "e3_alcpreg_yn_None:h_legume_preg_Ter(0.5,2]"          ,      "e3_alcpreg_yn_None:h_legume_preg_Ter(2,Inf]",               
                       "e3_alcpreg_yn_None:h_meat_preg_Ter(10,Inf]"            ,     "e3_alcpreg_yn_None:h_pavig_t3_NoneLow" ,                    
                       "e3_alcpreg_yn_None:h_pavig_t3_NoneMedium"               ,    "h_cereal_preg_Ter(9,27.3]:h_dairy_preg_Ter(17.1,27.1]" ,    
                       "h_cereal_preg_Ter(9,27.3]:h_dairy_preg_Ter(27.1,Inf]"    ,   "h_cereal_preg_Ter(27.3,Inf]:h_fastfood_preg_Ter(0.25,0.83]",
                       "h_cereal_preg_Ter(9,27.3]:h_fastfood_preg_Ter(0.25,0.83]" ,  "h_cereal_preg_Ter(27.3,Inf]:h_fastfood_preg_Ter(0.83,Inf]" ,
                       "h_cereal_preg_Ter(9,27.3]:h_fastfood_preg_Ter(0.83,Inf]"   , "h_cereal_preg_Ter(9,27.3]:h_fish_preg_Ter(1.9,4.1]"        ,
                       "h_cereal_preg_Ter(27.3,Inf]:h_fish_preg_Ter(4.1,Inf]"       ,"h_cereal_preg_Ter(9,27.3]:h_fish_preg_Ter(4.1,Inf]"        ,
                       "h_cereal_preg_Ter(27.3,Inf]:h_folic_t1_None"                ,"h_cereal_preg_Ter(9,27.3]:h_folic_t1_None"                 ,
                       "h_cereal_preg_Ter(27.3,Inf]:h_meat_preg_Ter(10,Inf]"        ,"h_cereal_preg_Ter(9,27.3]:h_meat_preg_Ter(6.5,10]"  ,       
                       "h_cereal_preg_Ter(27.3,Inf]:h_pamod_t3_NoneOften"           ,"h_cereal_preg_Ter(9,27.3]:h_pamod_t3_NoneOften"  ,          
                       "h_cereal_preg_Ter(9,27.3]:h_pamod_t3_NoneSometimes"         ,"h_cereal_preg_Ter(27.3,Inf]:h_pavig_t3_NoneLow" ,           
                       "h_cereal_preg_Ter(27.3,Inf]:h_pavig_t3_NoneMedium"          ,"h_dairy_preg_Ter(27.1,Inf]:h_fastfood_preg_Ter(0.25,0.83]" ,
                       "h_dairy_preg_Ter(17.1,27.1]:h_fish_preg_Ter(1.9,4.1]"       ,"h_dairy_preg_Ter(27.1,Inf]:h_fish_preg_Ter(1.9,4.1]"   ,    
                       "h_dairy_preg_Ter(17.1,27.1]:h_fish_preg_Ter(4.1,Inf]"       ,"h_dairy_preg_Ter(27.1,Inf]:h_fish_preg_Ter(4.1,Inf]",       
                       "h_dairy_preg_Ter(17.1,27.1]:h_folic_t1_None"                ,"h_dairy_preg_Ter(27.1,Inf]:h_folic_t1_None"     ,           
                       "h_dairy_preg_Ter(27.1,Inf]:h_fruit_preg_Ter(0.6,18.2]"      ,"h_dairy_preg_Ter(27.1,Inf]:h_legume_preg_Ter(0.5,2]"   ,    
                       "h_dairy_preg_Ter(17.1,27.1]:h_meat_preg_Ter(10,Inf]"        ,"h_dairy_preg_Ter(17.1,27.1]:h_pavig_t3_NoneLow"    ,        
                       "h_dairy_preg_Ter(27.1,Inf]:h_pavig_t3_NoneLow"              ,"h_dairy_preg_Ter(27.1,Inf]:h_pavig_t3_NoneMedium"  ,        
                       "h_fastfood_preg_Ter(0.25,0.83]:h_fish_preg_Ter(1.9,4.1]"    ,"h_fastfood_preg_Ter(0.83,Inf]:h_fish_preg_Ter(1.9,4.1]" ,   
                       "h_fastfood_preg_Ter(0.25,0.83]:h_folic_t1_None"             ,"h_fastfood_preg_Ter(0.83,Inf]:h_folic_t1_None" ,            
                       "h_fastfood_preg_Ter(0.25,0.83]:h_fruit_preg_Ter(0.6,18.2]"  ,"h_fastfood_preg_Ter(0.83,Inf]:h_fruit_preg_Ter(0.6,18.2]",  
                       "h_fastfood_preg_Ter(0.25,0.83]:h_legume_preg_Ter(2,Inf]"    ,"h_fastfood_preg_Ter(0.25,0.83]:h_meat_preg_Ter(10,Inf]" ,   
                       "h_fastfood_preg_Ter(0.83,Inf]:h_meat_preg_Ter(10,Inf]"      ,"h_fastfood_preg_Ter(0.25,0.83]:h_meat_preg_Ter(6.5,10]" ,   
                       "h_fastfood_preg_Ter(0.83,Inf]:h_meat_preg_Ter(6.5,10]"      ,"h_fastfood_preg_Ter(0.25,0.83]:h_pamod_t3_NoneOften"   ,    
                       "h_fastfood_preg_Ter(0.83,Inf]:h_pamod_t3_NoneOften"         ,"h_fastfood_preg_Ter(0.83,Inf]:h_pavig_t3_NoneLow"  ,        
                       "h_fastfood_preg_Ter(0.25,0.83]:h_pavig_t3_NoneMedium"       ,"h_fish_preg_Ter(1.9,4.1]:h_fruit_preg_Ter(0.6,18.2]" ,      
                       "h_fish_preg_Ter(1.9,4.1]:h_legume_preg_Ter(0.5,2]"          ,"h_fish_preg_Ter(4.1,Inf]:h_legume_preg_Ter(0.5,2]",         
                       "h_fish_preg_Ter(4.1,Inf]:h_legume_preg_Ter(2,Inf]"          ,"h_fish_preg_Ter(1.9,4.1]:h_meat_preg_Ter(10,Inf]" ,         
                       "h_fish_preg_Ter(4.1,Inf]:h_meat_preg_Ter(10,Inf]"           ,"h_fish_preg_Ter(1.9,4.1]:h_meat_preg_Ter(6.5,10]" ,         
                       "h_fish_preg_Ter(4.1,Inf]:h_meat_preg_Ter(6.5,10]"           ,"h_fish_preg_Ter(1.9,4.1]:h_pamod_t3_NoneOften"   ,          
                       "h_fish_preg_Ter(4.1,Inf]:h_pamod_t3_NoneOften"              ,"h_fish_preg_Ter(1.9,4.1]:h_pamod_t3_NoneSometimes",         
                       "h_fish_preg_Ter(1.9,4.1]:h_pamod_t3_NoneVery Often"         ,"h_fish_preg_Ter(1.9,4.1]:h_pavig_t3_NoneLow"   ,            
                       "h_fish_preg_Ter(4.1,Inf]:h_pavig_t3_NoneLow"                ,"h_folic_t1_None:h_fruit_preg_Ter(0.6,18.2]"  ,              
                       "h_folic_t1_None:h_legume_preg_Ter(0.5,2]"                   ,"h_folic_t1_None:h_meat_preg_Ter(10,Inf]"   ,                
                       "h_folic_t1_None:h_meat_preg_Ter(6.5,10]"                    ,"h_folic_t1_None:h_pamod_t3_NoneOften"    ,                  
                       "h_folic_t1_None:h_pamod_t3_NoneSometimes"                   ,"h_folic_t1_None:h_pavig_t3_NoneLow" ,                       
                       "h_fruit_preg_Ter(0.6,18.2]:h_meat_preg_Ter(10,Inf]"         ,"h_legume_preg_Ter(0.5,2]:h_meat_preg_Ter(10,Inf]" ,         
                       "h_legume_preg_Ter(2,Inf]:h_meat_preg_Ter(10,Inf]"           ,"h_legume_preg_Ter(0.5,2]:h_pamod_t3_NoneOften"   ,          
                       "h_legume_preg_Ter(2,Inf]:h_pamod_t3_NoneOften"              ,"h_legume_preg_Ter(0.5,2]:h_pamod_t3_NoneSometimes" ,        
                       "h_meat_preg_Ter(10,Inf]:h_pamod_t3_NoneOften"               ,"h_meat_preg_Ter(10,Inf]:h_pamod_t3_NoneSometimes" ,         
                       "h_meat_preg_Ter(6.5,10]:h_pamod_t3_NoneSometimes"           ,"h_meat_preg_Ter(10,Inf]:h_pavig_t3_NoneLow" ,               
                       "h_meat_preg_Ter(6.5,10]:h_pavig_t3_NoneLow"                 ,"h_pamod_t3_NoneOften:h_pavig_t3_NoneLow"   )





pred_lasso_inter <- data.frame(actual=c(test_data$hs_correct_raven),
                         predicted=c(predict(lm_lasso_inter, test_data, type="response")))

RMSE_lasso_inter<-rmse(pred_lasso_inter$actual, pred_lasso_inter$predicted)

pd_inter <- aov(data = preg_data, 
                hs_correct_raven~h_fastfood_preg_Ter*h_fish_preg_Ter)
summary(pd_inter)$d

interaction.plot(x.factor = preg_data$h_fish_preg_Ter, #x-axis variable
                 trace.factor = preg_data$h_fastfood_preg_Ter,#variable for lines
                 response = preg_data$hs_correct_raven, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "raven score",
                 xlab = "cat",
                 col = c("pink", "blue", "green",'black'),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Dairy")

#cereal and dairy
#legume and cereal
#folic acid and fish
#pavig and pamod has interaction
#look into fruit and pamod
#pavig and fruots no interaction
#fish and meat no interaction
#fast food and meat maybe
#fast food and fish maybe



##################Observing metrics
#Adjusted R^2
summary(lmfull)$adj.r.squared
summary(lm_lasso)$adj.r.squared
summary(lm_stepwise)$adj.r.squared
summary(lm_lasso_inter)$adj.r.sqaured#highest R squared
summary(lm_stepwise_inter)$adj.r.squared
#AIC
AIC(lmfull)#highest AIC
AIC(lm_lasso)
AIC(lm_stepwise)
AIC(lm_lasso_inter)#lowest AIC
AIC(lm_stepwise_inter)
#BIC
BIC(lmfull)
BIC(lm_lasso)
BIC(lm_stepwise)#lowest
BIC(lm_lasso_inter)#highest BIC
BIC(lm_stepwise_inter)


##Prediction
test_data<-read.csv('Data/test.csv')
test_data<-subset(test_data, select = -c(X,ID) )
test_data<-test_data[loc_preg]

predict(lm_stepwise, test_data, type="response")
mean((test_data$hs_correct_raven-predict(lm_stepwise, test_data, type="response")
)^2)


data_mod <- data.frame(Predicted = predict(lm_stepwise, test_data, type="response"),  # Create data for ggplot2
                       Observed = test_data$hs_correct_raven)








