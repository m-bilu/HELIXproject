library(glmnet)
setwd("M:/NexusDesktop/stat331-finalproject")
#load in data
data<-read.csv('Data/train.csv')
data<-subset(data, select = -c(X,ID) )
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
  random_sample <- createDataPartition(data$hs_correct_raven, p = 0.8, 
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
lm_lasso<-lm(validation_data$hs_correct_raven~e3_alcpreg_yn_None+h_cereal_preg_Ter+
             h_dairy_preg_Ter +h_fastfood_preg_Ter+
             h_fish_preg_Ter+ h_folic_t1_None+ 
             h_fruit_preg_Ter+h_legume_preg_Ter+ 
             h_meat_preg_Ter+
             h_pamod_t3_None+h_pavig_t3_None,data = validation_data)
#step-wise

system.time({
  Mstep <- step(object = lm,
                scope = list(lower = lm, upper = lmfull), 
                direction = "both", trace = 1)
})
lm_stepwise<-lm(validation_data$hs_correct_raven ~ h_cereal_preg_Ter + h_fastfood_preg_Ter + 
                  h_legume_preg_Ter + h_folic_t1_None,data = validation_data)

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

ggplot(data_mod,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)

mean((test_data$hs_correct_raven - predict(lm_stepwise,test_data,type = "response")) ^ 2)

#32.26023


###
data_mod2 <- data.frame(Predicted = predict(lm_lasso,test_data,type = "response"),  # Create data for ggplot2
                       Observed = test_data$hs_correct_raven)

ggplot(data_mod2,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)

mean((test_data$hs_correct_raven - predict(lm_lasso,test_data,type = "response")) ^ 2)
#31.25184

####
data_mod3 <- data.frame(Predicted = predict(lm_lasso_inter,test_data,type = "response"),  # Create data for ggplot2
                        Observed = test_data$hs_correct_raven)

ggplot(data_mod3,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
###
data_mod4 <- data.frame(Predicted = predict(lm_stepwise_inter,test_data,type = "response"),  # Create data for ggplot2
                        Observed = test_data$hs_correct_raven)

ggplot(data_mod4,                                     # Draw plot using ggplot2 package
       aes(x = Predicted,
           y = Observed)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
#55.99693



