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


###################
### Replicate measure without removing infrequent terms
###################

out2012thresh <- prepDocuments(processed2012$documents, processed2012$vocab, processed2012$meta)

out2012thresh$vocab
out2012$vocab[!out2012$vocab %in% out2012thresh$vocab]


###################
### Replicate measure with larger number of topics
###################

## stm fit with 50 topics (estimating number of topics gives ~70, but creates computational issues)
stm_fit2016_full <- stm(out2016$documents, out2016$vocab, prevalence = as.matrix(out2016$meta)
                        , K=40, init.type = "Spectral")

## combine sophistication components with remaining data
know <- sophistication(stm_fit2016_full, out2016)

## compute combined measures
data2016$polknow_text_mean_full <- (know$ntopics + know$distinct + data2016$ditem)/3


## combine sophistication components with remaining data
know <- sophistication(stm_fit2012_full, out2012)

## compute combined measures
data2012$polknow_text_mean_full <- (know$ntopics + know$distinct + data2012$ditem)/3

## stm fit with 50 topics (estimating number of topics gives ~70, but creates computational issues)
stm_fit2012_full <- stm(out2012$documents, out2012$vocab, prevalence = as.matrix(out2012$meta)
                        , K=40, init.type = "Spectral")


## export results
save(preprocessed_documents2012, preprocessed_documents2016
     , preText_results2012, preText_results2016
     , file="out/anes_pretext.Rdata")
