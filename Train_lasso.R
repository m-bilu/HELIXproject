# 
# Input data -> receive training and testing data
# 
# Usage:
# train_and_test <- train_test(the_complete_dataset)
# train <- train_and_test$train
# test <- train_and_test$test
train_test <- function(data) {
  library(caret)
  set.seed(331)
  random_sample <- createDataPartition(data$hs_correct_raven, p = 0.8, 
                                       list = FALSE)
  training_data  <- data[random_sample, ]
  testing_data <- data[-random_sample, ]
  list(train = training_data, test = testing_data)
}

# 
# Input training dataset and all subset of covariates specific to the use case and receiving reduced covariates
#
# Usage:
# reduced_covariates <- selection_lasso(train, covariates)
selection_lasso <- function(train, covariates) {
  set.seed(331)
  library(glmnet)
  y <- train$hs_correct_raven
  x <- data.matrix(train[,covariates])
  cv_model <- cv.glmnet(x, y, alpha = 1)
  best_lambda <- cv_model$lambda.min
  plot(cv_model)
  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  coef(best_model)
  ind <- which(coef(best_model)==0)
  covariates[-ind]
}
