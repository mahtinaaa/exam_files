#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(readr)
library(kknn)
library(ggplot2)
library(purrr)
library(dplyr)
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
rf_model_reg <- rand_forest() %>% 
  set_mode("regression") %>% 
  set_engine("ranger")

liver_rf_wf <- workflow() %>% 
  add_recipe(riskfactor_recipe) %>% 
  add_model(rf_model_reg)

liver_rf_fit <- fit(liver_rf_wf, liver_training)

riskfactor_rf_prediction = liver_rf_fit %>%
  predict(liver_testing) %>%
  bind_cols(liver_testing)

riskfactor_rf_prediction %>%
  ggplot(aes(x=Fatty_Liver_Disease_Risk_Score, y=.pred))+
  geom_point(alpha = 0.4, colour = "blue")+
  geom_abline(colour = "red", alpha = 0.9)
