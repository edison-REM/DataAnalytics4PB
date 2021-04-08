# Load packages necessary for this script (install first if necessary)
library(tidyverse)
library(caret)
library(party)
library(partykit)

# Read college data
college_train <- read_csv(file = "1_Data/college_train.csv")
college_test <- read_csv(file = "1_Data/college_test.csv")

# Convert all character features to factor
college_train <- college_train %>% mutate_if(is.character, factor) 
college_test <- college_test %>% mutate_if(is.character, factor)

# Use 10-fold cross validation
ctrl_cv <- trainControl(method = "cv", number = 10) 

### STANDARD REGRESSION --------

# Standard regression 
graduation_glm <- train(form = Grad.Rate ~ .,
                        data = college_train,
                        method = "glm",
                        trControl = ctrl_cv)

# Show final model
graduation_glm$finalModel

# Save fitted values
glm_fit <- predict(graduation_glm)

### LASSO REGRESSION --------

# Vector of lambda values to try
lambda_vec <- 10 ^ (seq(-3, 1, length = 100))

# Lasso regression
graduation_lasso <- train(form = Grad.Rate ~ .,
                           data = college_train,
                           method = "glmnet",
                           trControl = ctrl_cv,
                           preProcess = c("center", "scale"), 
                           tuneGrid = expand.grid(alpha = 1,  
                                                  lambda = lambda_vec)) 

# Show best parameters
plot(graduation_lasso)

# Get coefficients from best lambda value
coef(graduation_lasso$finalModel, 
     graduation_lasso$bestTune$lambda)

# Save fitted values
lasso_fit <- predict(graduation_lasso)

### DECISION TREE --------

# Determine possible values for cp
cp_vec <- seq(from = 0, to = .2, length = 100)

# Decision tree
graduation_rpart <- train(form = Grad.Rate ~ .,
                          data = college_train,
                          method = "rpart",
                          trControl = ctrl_cv,
                          tuneGrid = data.frame(cp = cp_vec))

# Show best cp
plot(graduation_rpart)

# Visualise your tree
plot(as.party(graduation_rpart$finalModel)) 

# Save fitted values
rpart_fit <- predict(graduation_rpart)

### RANDOM FOREST --------

# mtry candidates
mtry_vec <- 1:5

# Random forest
graduation_rf <- train(form = Grad.Rate ~ .,
                       data = college_train,
                       method = "rf",
                       trControl = ctrl_cv,
                       tuneGrid = data.frame(mtry = mtry_vec))

# show best mtry
plot(graduation_rf)

# Save fitted values
rf_fit <- predict(graduation_rpart)

### EVALUATE FIT --------

# store criterion
criterion_train <- college_train$Grad.Rate

# evaluate fit
postResample(pred = glm_fit, obs = criterion_train)
postResample(pred = lasso_fit, obs = criterion_train)
postResample(pred = rpart_fit, obs = criterion_train)
postResample(pred = rf_fit, obs = criterion_train)

### EVALUATE PREDICTION --------

# store criterion
criterion_test <- college_test$Grad.Rate

# new predictions
glm_fit <- predict(graduation_glm, newdata = college_test)
lasso_fit <- predict(graduation_lasso, newdata = college_test)
rpart_fit <- predict(graduation_rpart, newdata = college_test)
rf_fit <- predict(graduation_rf, newdata = college_test)

# evaluate fit
postResample(pred = glm_fit, obs = criterion_test)
postResample(pred = lasso_fit, obs = criterion_test)
postResample(pred = rpart_fit, obs = criterion_test)
postResample(pred = rf_fit, obs = criterion_test)






