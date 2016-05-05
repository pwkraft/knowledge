### ============================= ###
### Text as Data - Final Project  ###
### Laura Buchanan, Patrick Kraft ###
### ============================= ###
## This file models the 2008 and 2012 ANES with STM

### load raw data

rm(list = ls())
library(car)
library(dplyr)
library(quanteda)
library(stm)
library(foreign)
library(corrplot)

setwd('/Users/Laura/Desktop/knowledge/data')

load("../data/anes.Rdata")
meta_data <- anes2012
data <- anes2012spell
data <- data.frame(caseid=data$caseid, response = apply(data[,-1],1,paste,collapse=' '))

processed <- textProcessor(t(data), metadata = meta_data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

out <- subset(out,out$meta$spanish == 0)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# quick fit
#stm_fit <- stm(docs, vocab, K = 20, max.em.its = 10)

# slow, smart fit  
stm_fit <- stm(docs, vocab, K=0, init.type = "Spectral")

# Graphical display of estimated topic proportions 
#plot.STM(stm_fit, type = "summary")
#plot.STM(stm_fit, type = "perspectives", topics = c(18,20))

topic_words <- labelTopics(stm_fit)

# word count for each document
doc_wc <- vector("numeric", length(docs))
for (doc in 1:length(docs)){
  doc_string <- toString(docs[doc])
  doc_string_len <- length(unlist(strsplit(doc_string," ")))
  doc_wc[doc] <- doc_string_len 
}  

# probably fits and tranform for diversity score  
doc_topic_prob <- stm_fit$theta
doc_topic_prob_log2 <- log2(doc_topic_prob)

# topic diversity score
doc_topic_diversity <- doc_topic_prob * doc_topic_prob_log2
doc_topic_diversity <- -1 * rowSums(doc_topic_diversity, na.rm = FALSE, dims = 1)

# weighted topic diversity score, by length
doc_topic_diversity_len_weight <- doc_topic_diversity / log(doc_wc)

# threshold topic probability scores
doc_topic_prob_sparse <- doc_topic_prob
doc_topic_prob_sparse[doc_topic_prob_sparse < 0.1] <- 0
doc_topic_prob_binary <- doc_topic_prob
doc_topic_prob_binary[doc_topic_prob_binary < 0.1] <- 0
doc_topic_prob_binary[doc_topic_prob_binary > 0.1] <- 1

# count over thresholded prob scores
doc_topic_thresh_sum <- rowSums(doc_topic_prob_sparse, na.rm = FALSE, dims = 1)
doc_topic_thresh_count <- rowSums(doc_topic_prob_binary, na.rm=FALSE, dims = 1)

# knowledge metrics
know1 <- meta$polknow_office
know2 <- meta$polknow_factual
know3 <- meta$polknow_majority
know4 <- meta$polknow_evalpre
know5 <- meta$polknow_evalpost

# correlation matrix
corrm <- cor(cbind(know1,know2,know3,know4,know5
          ,doc_wc,doc_topic_diversity,doc_topic_diversity_len_weight
          ,doc_topic_thresh_sum,doc_topic_thresh_count)
         ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")




