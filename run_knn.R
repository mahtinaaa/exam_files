#!/usr/bin/env Rscript

library(tidyverse)
library(tidymodels)
library(readr)
library(kknn)
library(ggplot2)
args   = commandArgs(trailingOnly=TRUE)
input  = args[1]
cores  = args[2]
output = args[3]

writeLines("Load the dataset")
liver_dataset = read_tsv("/home/students/mdb2024s32/Final_exam/Predicting_Fatty_Liver_Disease_Risk_Score.tsv")
data_modified <- liver_dataset[, c("ALT", "triglycerides", "bilirubin", "AST", "albumin", "basophils", "Fatty_Liver_Disease_Risk_Score")]
set.seed(123)
data_split <- initial_split(data_modified, prop = 0.8)

liver_training <- training(data_split)
liver_testing <- testing(data_split)
riskfactor_recipe <- recipe(Fatty_Liver_Disease_Risk_Score ~ ALT + triglycerides + bilirubin + AST + albumin + basophils, data = liver_training) %>%
  step_normalize()
knn_reg_model <-
  nearest_neighbor(neighbors = 5, weight_func = "triangular") %>%
  set_mode("regression") %>%
  set_engine("kknn")

liver_wf_knn <- workflow() %>% 
  add_recipe(riskfactor_recipe) %>% 
  add_model(knn_reg_model)

liver_fit_knn <- fit(
  liver_wf_knn,
  liver_training
)

riskfactor_prediction_knn = liver_fit_knn %>%
  predict(liver_testing) %>%
  bind_cols(liver_testing)

riskfactor_prediction_knn %>%
  ggplot(aes(x=Fatty_Liver_Disease_Risk_Score, y=.pred))+
  geom_point(alpha = 0.4, colour = "orange")+
  geom_abline(colour = "red", alpha = 0.9)
