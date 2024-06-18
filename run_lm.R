#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(vip)

args   = commandArgs(trailingOnly=TRUE)
input  = args[1]
cores  = args[2]
output = args[3]

writeLines("Load the dataset")
liver_dataset = readRDS(input)
data_modified <- liver_dataset[, c("ALT", "triglycerides", "bilirubin", "AST", "albumin", "basophils", "Fatty_Liver_Disease_Risk_Score")]
set.seed(123)
data_split <- initial_split(data_modified, prop = 0.8)

liver_training <- training(data_split)
liver_testing <- testing(data_split)
riskfactor_recipe <- recipe(Fatty_Liver_Disease_Risk_Score ~ ALT + triglycerides + bilirubin + AST + albumin + basophils, data = liver_training) %>%
  step_normalize()

# Define the linear regression model
lm_model <- linear_reg() %>% 
  set_engine("lm")

# Define the recipe for preprocessing
riskfactor_recipe <- recipe(Fatty_Liver_Disease_Risk_Score ~ ALT + triglycerides + bilirubin + AST + albumin + basophils, data = liver_training) %>%
  step_normalize()

# Create the workflow
predicting_lm_wf <- workflow() %>% 
  add_recipe(riskfactor_recipe) %>% 
  add_model(lm_model)

# Fit the model
riskfactor_lm_formula_fit <- fit(predicting_lm_wf, liver_training)

# Display the model summary
tidy(riskfactor_lm_formula_fit)

# Make predictions on the testing set
riskfactor_lm_prediction <- riskfactor_lm_formula_fit %>% 
  predict(liver_testing) %>% 
  bind_cols(liver_testing)

# Plot the predictions
riskfactor_lm_prediction %>%
  ggplot(aes(x = Fatty_Liver_Disease_Risk_Score, y = .pred)) +
  geom_point(alpha = 0.4, colour = "green") +
  geom_abline(colour = "red", alpha = 0.9)
