# Load packages necessary for this script (install first if necessary)
library(tidyverse)
library(caret)
library(party)
library(partykit)

# Read college data
college_train <- read_csv(file = "1_Data/college_train.csv")

# Convert all character features to factor
college_train <- college_train %>% mutate_if(is.character, factor) 

# Set control to none
ctrl <- trainControl(method = "cv") 

### STANDARD REGRESSION --------

# Standard regression 
graduation_glm <- train(form = Grad.Rate ~ .,
                        data = college_train,
                        method = "glm",
                        trControl = ctrl)

# Show final model
graduation_glm$finalModel

# Save fitted values
glm_fit <- predict(graduation_glm)

### DECISION TREE --------

# Decision tree
graduation_rpart <- train(form = Grad.Rate ~ .,
                          data = college_train,
                          method = "rpart",
                          trControl = ctrl_cv,
                          tuneGrid = data.frame(cp = cp_vec))


# Save fitted values
rpart_fit <- predict(graduation_rpart)

### RANDOM FOREST --------

# Random forest
graduation_rf <- train(form = Grad.Rate ~ .,
                       data = college_train,
                       method = "rf",
                       trControl = ctrl)

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

# Read college test data
college_test <- read_csv(file = "1_Data/college_test.csv")

# Convert all character features to factor
college_test <- college_test %>% mutate_if(is.character, factor)

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




