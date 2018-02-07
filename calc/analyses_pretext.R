### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## preText analysis of ANES data (based on data created in prep.R)


rm(list = ls())
gc()

library(quanteda)
library(stm)
library(ggplot2)
library(gridExtra)
library(preText)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

## load data and stm results
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")

## only select documents
resp2012 <- data2012$resp
resp2016 <- data2016$resp
rm(list=ls()[!ls()%in%c("resp2012","resp2016")])

## preprocess data
preprocessed_documents2012 <- factorial_preprocessing(
  resp2012[1:500], use_ngrams = FALSE, parallel = TRUE, cores = 4
  , intermediate_directory = "out")
preprocessed_documents2016 <- factorial_preprocessing(
  resp2016[1:500], use_ngrams = FALSE, parallel = TRUE, cores = 4
  , intermediate_directory = "out")

## run preText
preText_results2012 <- preText(
  preprocessed_documents2012, dataset_name = "2012 ANES", parallel = TRUE, cores = 4)
preText_results2016 <- preText(
  preprocessed_documents2016, dataset_name = "2016 ANES", parallel = TRUE, cores = 4)

## generate preText score plot
preText_score_plot(preText_results2012)
preText_score_plot(preText_results2016)

## generate regression results
regression_coefficient_plot(
  preText_results2012, remove_intercept = TRUE)
regression_coefficient_plot(
  preText_results2016, remove_intercept = TRUE)

## export results
save(preprocessed_documents2012, preprocessed_documents2016
     , preText_results2012, preText_results2016
     , file="out/anes_pretext.Rdata")
