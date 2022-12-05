load("/Users/srijanchaudhuri/Downloads/exposome_NA.RData")

df <- read.csv("/Users/srijanchaudhuri/Downloads/full_clean_data_v2.csv")

drop <- c("ID","X")

all_data = df[,!(names(df) %in% drop)]

numcat_cols <- c("e3_alcpreg_yn_None", "h_folic_t1_None", "h_cohort",
                 "e3_yearbir_None", #"h_edumc_None", 
                 "h_native_None", 
                 "h_parity_None", "h_accesslines300_preg_dic0",
                 "hs_accesslines300_h_dic0", "hs_accesslines300_s_dic0",
                 "hs_pet_cat_r2_None", "hs_pet_dog_r2_None", 
                 "hs_blueyn300_s_None", "h_blueyn300_preg_None",
                 "h_greenyn300_preg_None", "hs_greenyn300_s_None",
                 "hs_blueyn300_h_None", "hs_greenyn300_h_None",
                 "hs_lden_cat_s_None")

#all_data[, "h_cohort"]<- as.factor(all_data[, "h_cohort"]) 

columns_make_factor <- function(data, covariates) {
  for (i in covariates){
    data[,i] <- as.factor(data[,i])
  }
  data
}

all_data <- columns_make_factor(all_data, numcat_cols)

very_suss_cols <- c("h_edumc_None", "h_native_None", 
               "h_parity_None","h_accesspoints300_preg_Log")

potentiall_suss_cols <- c("hs_c_weight_None", "hs_as_m_Log2")

covariates_full <- as.vector(
  codebook$variable_name[which(codebook$domain != "Phenotype") ])

covariates_life <- as.vector(
  codebook$variable_name[which(codebook$family == "Lifestyle" 
                               & codebook$domain != "Phenotype") ])
covariates_preg <- as.vector(
  codebook$variable_name[which(codebook$period == "Pregnancy"
                               & codebook$family == "Lifestyle"
                               & codebook$domain != "Phenotype")])
covariates_post <- as.vector(
  codebook$variable_name[which(codebook$period == "Postnatal"
                               & codebook$family == "Lifestyle"
                               & codebook$domain != "Phenotype")])

train_test <- function(data) {
  library(caret)
  set.seed(331)
  random_sample <- createDataPartition(data$hs_correct_raven, p = 0.8, 
                                       list = FALSE)
  training_data  <- data[random_sample, ]
  testing_data <- data[-random_sample, ]
  list(train = training_data, test = testing_data)
}

train_and_test <- train_test(all_data)
train <- train_and_test$train
test <- train_and_test$test

write.csv(train, file = '/Users/srijanchaudhuri/Desktop/train.csv',
          row.names = FALSE)
write.csv(test, file = '/Users/srijanchaudhuri/Desktop/test.csv',
          row.names = FALSE)

selection_lasso <- function(train, covariates) {
  set.seed(331)
  library("dplyr")
  y <- train$hs_correct_raven
  x <- data.matrix(train[, covariates])
  library(glmnet)
  cv_model <- cv.glmnet(x, y, alpha = 1)
  best_lambda <- cv_model$lambda.min
  plot(cv_model)
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  v <- which(as.vector(coef(best_model))!=0)
  covariates[v[2:length(v)] - 1]
}

reduced_full <- selection_lasso(train, covariates_full)
reduced_life <- selection_lasso(train, covariates_life)
reduced_post <- selection_lasso(train, covariates_post)
reduced_preg <- selection_lasso(train, covariates_preg)

# Don't include 100+ covariates, do EDA first, narrow covariates and then use,
# prem parameter in the last rejects every p-value with p > 0.1
selection_backward <- function(train, covariates) {
  library(olsrr)
  frmla <- as.formula(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), sep=""))
  model <- lm(frmla, data=train)
  ols_step_backward_p(model, prem=0.1)
}

selection_forward <- function(train, covariates) {
  library(olsrr)
  frmla <- as.formula(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), sep=""))
  model <- lm(frmla, data=train)
  ols_step_forward_p(model, p=0.05)
}


selection_stepwise <- function(train, covariates) {
  library(MASS)
  frmla <- as.formula(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), 
                            sep=""))
  model <- lm(frmla, data = train)
  stepAIC(model, direction = "both", trace = FALSE)
}

make_linear <- function(train, covariates){
  frmla <- as.formula(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), 
                            sep=""))
  model <- lm(frmla, data=train)
  model
}

testing_rmse <- function(test, model, covariates) {
  library(Metrics)
  preds <- as.vector(predict(model,test[, covariates]))
  print(rmse(test$hs_correct_raven, preds))
}

get_interactions <- function(covariates) {
  l <- length(covariates)
  vec <- rep("", times=l*l)
  for (i in 1:l) {
    for(j in 1:l) {
      if (covariates[i]!=covariates[j]) {
        if (covariates[i] > covariates[j]) {
          vec[(l*(i - 1)) + j] = paste(covariates[i],covariates[j], sep=":")
        } else {
          vec[(l*(i - 1)) + j] = paste(covariates[j],covariates[i], sep=":")
        }
      }
    }
  }
  new_vec <- unique(vec)
  new_vec[!new_vec %in% ""]
}

make_linear_with_interactions <- function(train, covariates) {
  frmla <- as.formula(paste(paste("hs_correct_raven ~ ", 
                            paste(covariates, collapse = "+"), sep=""),
                            paste(get_interactions(covariates), collapse ="+"), 
                            sep="+"))
  model <- lm(frmla, data=train)
  model
}

m_full <- make_linear(train, reduced_full)
m_life <- make_linear(train, reduced_life)
m_preg <- make_linear(train, reduced_preg)
m_post <- make_linear(train, reduced_post)

testing_rmse(test, m_full, reduced_full)
testing_rmse(test, m_life, reduced_life)
testing_rmse(test, m_preg, reduced_preg)
testing_rmse(test, m_post, reduced_post)

error_normality_diagnosis <- function(model) {
  res <- resid(model)
  stud <- res/(sigma(model)*sqrt(1-hatvalues(model)))
  par(mfrow=c(1,2))
  hist(stud,breaks=12,
       probability=TRUE,xlim=c(-4,4),
       xlab="Studentized Residuals",
       main="Distribution of Residuals")
  grid <- seq(-3.5,3.5,by=0.05)
  lines(x=grid,y=dnorm(grid),col="blue")
  qqnorm(stud)
  abline(0,1, col="red")
}

error_diagnosis(m_full)
error_diagnosis(m_life)
error_diagnosis(m_post)
error_diagnosis(m_preg)

error_variability_diagnosis <- function(model) {
  res <- resid(model)
  fit <- fitted(model)
  plot(res~fit, xlab="Fitted Vals", ylab="Studentized Residuals",
       main="Residuals vs Fitted")
}

error_linearity_diagnosis <- function(model, train) {
  res <- resid(model)
  plot(res~train$hs_correct_raven, xlab="Raven Score", ylab="Residuals",
       main="Residuals vs X")
}

plot_res <- function(model, train, var) {
  res <- resid(model)
  plot(res~train[,var], xlab="x", ylab="Residuals",
       main="Residuals vs X")
}

