### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## runs analyses for paper (based on data created in prep.R)


rm(list = ls())
library(dplyr)
library(quanteda)
library(stm)
library(corrplot)
library(ggplot2)

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}

## load data and stm results
load("../data/anes.Rdata")

## Graphical display of estimated topic proportions 
plot.STM(stm_fit, type = "summary")
plot.STM(stm_fit, type = "perspectives", topics = c(18,20))
topic_words <- labelTopics(stm_fit)


# correlation matrix
corrm <- cor(data[,c("polknow_office", "polknow_factual", "polknow_majority", "polknow_evalpre"
                  , "polknow_evalpost", "intpre", "intpost", "educ_cont", "polmedia", "poldisc"
                  , "wc", "lwc", "nitem", "pitem", "litem", "ditem", "topic_diversity"
                  , "topic_diversity_length", "topic_thresh_sum", "topic_thresh_count"
                  , "topic_diversity_length_litem", "topic_diversity_length_ditem")]
         ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")

## the diversity measure is still a bit problematic since it's high for individuals with few words

## histogram/density of dv
ggplot(data, aes(topic_diversity_length_ditem, ..density..)) + geom_histogram(fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA))

## alternative dvs:
ggplot(data, aes(topic_diversity, ..density..)) + geom_histogram(fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA))
ggplot(data, aes(topic_diversity_length, ..density..)) + geom_histogram(fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA))
ggplot(data, aes(topic_diversity_length_litem, ..density..)) + geom_histogram(fill='grey') + geom_density() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA))

### test some models
m1 <- NULL
m1[[1]] <- lm(topic_diversity_length_ditem ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[2]] <- lm(polknow_office ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[3]] <- lm(polknow_factual ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[4]] <- lm(polknow_majority ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont + mode, data = data)
m1[[5]] <- lm(polknow_evalpre ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)
m1[[6]] <- lm(polknow_evalpost ~ polmedia + poldisc + educ_cont + female + age + black + relig + ideol_ct + pid_cont, data = data)

lapply(m1, summary)
