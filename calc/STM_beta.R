### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## This file models the 2012 ANES with STM


### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(quanteda)
library(stm)
library(foreign)
library(corrplot)
library(ggplot2)

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}

load("../data/anes.Rdata")


### prepare data

## combine regular survey and open-ended data, remove spanish and empty responses
meta <- c("age", "educ_cont", "pid_cont", "educ_pid")
data <- anes2012 %>% mutate(resp = apply(anes2012spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

## remove missings on metadata
data <- data[apply(!is.na(data[,meta]),1,prod)==1,]

## process for stm
processed <- textProcessor(data$resp, metadata = data[,meta])
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

## quick fit
# stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
#                , K=60, init.type = "Spectral")

## slow, smart fit: estimates about 80 topics, might be too large (also, K is not deterministic here)
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta)
               , K=0, init.type = "Spectral")

## Graphical display of estimated topic proportions 
plot.STM(stm_fit, type = "summary")
plot.STM(stm_fit, type = "perspectives", topics = c(18,20))
topic_words <- labelTopics(stm_fit)

# probability fits and transform for diversity score  
doc_topic_prob <- stm_fit$theta

# topic diversity score
data$topic_diversity <- -1 * rowSums(doc_topic_prob * log2(doc_topic_prob))

# weighted topic diversity score, by length
data$topic_diversity_length <- with(data, (topic_diversity + 1) * (lwc + 1))

# threshold topic probability scores
doc_topic_prob_sparse <- doc_topic_prob
doc_topic_prob_sparse[doc_topic_prob_sparse < 0.1] <- 0
doc_topic_prob_binary <- doc_topic_prob
doc_topic_prob_binary[doc_topic_prob_binary < 0.1] <- 0
doc_topic_prob_binary[doc_topic_prob_binary > 0.1] <- 1

# count over thresholded prob scores
data$topic_thresh_sum <- rowSums(doc_topic_prob_sparse)
data$topic_thresh_count <- rowSums(doc_topic_prob_binary)

# weighted by item count + diversity
data$topic_diversity_length_litem <- with(data, (topic_diversity + 1) * (lwc + 1) * (litem + 1))
data$topic_diversity_length_ditem <- with(data, (topic_diversity + 1) * (lwc + 1) * (ditem + 1))

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

