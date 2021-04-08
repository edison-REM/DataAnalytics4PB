# Load packages necessary for this script (install first if necessary)
library(tidyverse)
library(caret)
library(party)
library(partykit)

# Read college data
college_train <- read_csv(file = "1_Data/college_train.csv")

# TASKS ------
# 1: Run regular regression using lm() predicting Grad.Rate with all other variables
# 2: Inspect estimated parameters: Which variables "predic" Grad.Rate
# 3: What is the overall performance of the model in terms of R-squared


