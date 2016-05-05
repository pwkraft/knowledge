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

if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
  setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")
} else {setwd('/Users/Laura/Desktop/knowledge/data')}

load("../data/anes.Rdata")


### prepare data

## select meta data, remove spanish + missing, etc.
data <- anes2012 %>% select(caseid, age, educ_cont, pid_cont) %>%
  mutate(educ_pid = educ_cont * pid_cont
         , resp = apply(anes2012spell[,-1],1,paste,collapse=' ')) %>%
  filter(anes2012$spanish == 0 & anes2012$wc != 0) %>% na.omit()

## process for stm
processed <- textProcessor(data$resp, metadata = select(data, -resp, -caseid))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# quick fit
#stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta), K=20, init.type = "Spectral")

# slow, smart fit  
stm_fit <- stm(out$documents, out$vocab, prevalence = as.matrix(out$meta), K=0, init.type = "Spectral")

# Graphical display of estimated topic proportions 
#plot.STM(stm_fit, type = "summary")
#plot.STM(stm_fit, type = "perspectives", topics = c(18,20))

topic_words <- labelTopics(stm_fit)


# probably fits and tranform for diversity score  
doc_topic_prob <- stm_fit$theta
doc_topic_prob_log2 <- log2(doc_topic_prob)

# topic diversity score
doc_topic_diversity <- doc_topic_prob * doc_topic_prob_log2
doc_topic_diversity <- -1 * rowSums(doc_topic_diversity, na.rm = FALSE, dims = 1)

# weighted topic diversity score, by length
doc_wc <- anes2012$wc[anes2012$caseid %in% data$caseid]
doc_topic_diversity_len_weight <- doc_topic_diversity / (log(doc_wc) + 1)

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
know1 <- anes2012$polknow_office[anes2012$caseid %in% data$caseid]
know2 <- anes2012$polknow_factual[anes2012$caseid %in% data$caseid]
know3 <- anes2012$polknow_majority[anes2012$caseid %in% data$caseid]
know4 <- anes2012$polknow_evalpre[anes2012$caseid %in% data$caseid]
know5 <- anes2012$polknow_evalpost[anes2012$caseid %in% data$caseid]



# correlation matrix
corrm <- cor(cbind(know1,know2,know3,know4,know5
          ,doc_wc,doc_topic_diversity,doc_topic_diversity_len_weight
          ,doc_topic_thresh_sum,doc_topic_thresh_count)
         ,use="pairwise.complete.obs", method = "pearson")

corrplot(corrm,method="square")




