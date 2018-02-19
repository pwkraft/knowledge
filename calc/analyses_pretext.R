### ============================================================= ###
### Measuring Political Sophistication using Open-ended Responses ###
### Patrick Kraft                                                 ###
### ============================================================= ###
## preText analysis of ANES data (based on data created in prep.R)


rm(list = ls())
gc()

library(dplyr)
library(quanteda)
library(stm)
library(ggplot2)
library(gridExtra)
library(preText)

setwd("/data/Dropbox/Uni/Projects/2016/knowledge/calc")

source("func.R")
plot_default <- theme_classic(base_size=9) + theme(panel.border = element_rect(fill=NA))

## load data and stm results
load("out/anes2012.Rdata")
load("out/anes2016.Rdata")
load("out/yougov.Rdata")
load("out/swiss.Rdata")

## select raw documents in anes
opend2012 <- apply(anes2012opend[anes2012opend$caseid %in% data2012$caseid[1:500], -1]
                   , 1, paste, collapse=' ')
opend2016 <- apply(anes2016opend[anes2016opend$V160001 %in% data2016$caseid[1:500], -1]
                   , 1, paste, collapse=' ')
opend_yougov <- apply(opend[opend$caseid %in% data_yg$caseid[1:500], -1]
                      , 1, paste, collapse=' ')
opend_german <- apply(cbind(opend_german$string1[1:500],opend_german$string2[1:500])
                      , 1, paste, collapse=' ')
opend_french <- apply(cbind(opend_french$string1[1:500],opend_french$string2[1:500])
                      , 1, paste, collapse=' ')
opend_italian <- apply(cbind(opend_italian$string1[1:500],opend_italian$string2[1:500])
                       , 1, paste, collapse=' ')

## preprocess data
preprocessed_documents2012 <- factorial_preprocessing(
  opend2012, use_ngrams = FALSE, parallel = TRUE, cores = 7)
preprocessed_documents2016 <- factorial_preprocessing(
  opend2016, use_ngrams = FALSE, parallel = TRUE, cores = 7)
preprocessed_documents_yougov <- factorial_preprocessing(
  opend_yougov, use_ngrams = FALSE, parallel = TRUE, cores = 7)
preprocessed_documents_german <- factorial_preprocessing(
  opend_german, use_ngrams = FALSE, parallel = TRUE, cores = 7)
preprocessed_documents_french <- factorial_preprocessing(
  opend_french, use_ngrams = FALSE, parallel = TRUE, cores = 7)
preprocessed_documents_italian <- factorial_preprocessing(
  opend_italian, use_ngrams = FALSE, parallel = TRUE, cores = 7)

## remove intermediate dfms
file.remove(dir()[grep("intermediate_dfm_\\d+\\.Rdata", dir())])

## run preText
preText_results2012 <- preText(preprocessed_documents2012
                               , dataset_name = "2012 ANES", parallel = TRUE, cores = 7)
preText_results2016 <- preText(preprocessed_documents2016
                               , dataset_name = "2016 ANES", parallel = TRUE, cores = 7)
preText_results_yougov <- preText(preprocessed_documents_yougov
                                  , dataset_name = "2015 YouGov", parallel = TRUE, cores = 7)
preText_results_german <- preText(preprocessed_documents_german
                                  , dataset_name = "Swiss (German)", parallel = TRUE, cores = 7)
preText_results_french <- preText(preprocessed_documents_french
                                  , dataset_name = "Swiss (French)", parallel = TRUE, cores = 7)
preText_results_italian <- preText(preprocessed_documents_italian
                                   , dataset_name = "Swiss (Italian)", parallel = TRUE, cores = 7)

## generate preText score plot
preText_score_plot(preText_results2012)
preText_score_plot(preText_results2016)
preText_score_plot(preText_results_yougov)
preText_score_plot(preText_results_german)
preText_score_plot(preText_results_french)
preText_score_plot(preText_results_italian)

## generate regression results
pdf("../fig/pretext2012.pdf", width=5, height=2)
regression_coefficient_plot(preText_results2012, remove_intercept = TRUE)
dev.off()

pdf("../fig/pretext2016.pdf", width=5, height=2)
regression_coefficient_plot(preText_results2016, remove_intercept = TRUE)
dev.off()

pdf("../fig/pretext_yougov.pdf", width=5, height=2)
regression_coefficient_plot(preText_results_yougov, remove_intercept = TRUE)
dev.off()

pdf("../fig/pretext_german.pdf", width=5, height=2)
regression_coefficient_plot(preText_results_german, remove_intercept = TRUE)
dev.off()

pdf("../fig/pretext_french.pdf", width=5, height=2)
regression_coefficient_plot(preText_results_french, remove_intercept = TRUE)
dev.off()

pdf("../fig/pretext_italian.pdf", width=5, height=2)
regression_coefficient_plot(preText_results_italian, remove_intercept = TRUE)
dev.off()



###################
### Replicate measure with smaller number of topics
###################


### 2012 ANES

## stm fit with 20 topics
stm_fit2012_ktopic <- stm(out2012$documents, out2012$vocab, prevalence = as.matrix(out2012$meta)
                          , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit2012_ktopic, out2012)

## compute combined measures
data2012$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + data2012$ditem)/3


### 2016 ANES

## stm fit with 20 topics
stm_fit2016_ktopic <- stm(out2016$documents, out2016$vocab, prevalence = as.matrix(out2016$meta)
                        , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit2016_ktopic, out2016)

## compute combined measures
data2016$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + data2016$ditem)/3


### 2015 YouGov

## stm fit with 20 topics
stm_fit_yg_ktopic <- stm(out_yougov$documents, out_yougov$vocab, prevalence = as.matrix(out_yougov$meta)
                          , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit_yg_ktopic, out_yougov)

## compute combined measures
data_yg$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + data_yg$ditem)/3


### Swiss (German)

## stm fit with 20 topics
stm_fit_german_ktopic <- stm(out_german$documents, out_german$vocab, prevalence = as.matrix(out_german$meta)
                         , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit_german_ktopic, out_german)

## compute combined measures
opend_german$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + opend_german$ditem)/3


### Swiss (French)

## stm fit with 20 topics
stm_fit_french_ktopic <- stm(out_french$documents, out_french$vocab, prevalence = as.matrix(out_french$meta)
                             , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit_french_ktopic, out_french)

## compute combined measures
opend_french$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + opend_french$ditem)/3


### Swiss (italian)

## stm fit with 20 topics
stm_fit_italian_ktopic <- stm(out_italian$documents, out_italian$vocab, prevalence = as.matrix(out_italian$meta)
                             , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit_italian_ktopic, out_italian)

## compute combined measures
opend_italian$polknow_text_mean_ktopic <- (know$ntopics + know$distinct + opend_italian$ditem)/3



###################
### Replicate measure without removing infrequent terms
###################


### 2012 ANES
gc()

## combine regular survey and open-ended data, remove spanish and empty responses
data2012_thresh <- anes2012 %>% mutate(resp = apply(anes2012spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

## remove missings on metadata
data2012_thresh <- data2012_thresh[apply(!is.na(data2012_thresh[,meta2012]),1,prod)==1,]

## process for stm
out2012_thresh <- prepDocuments(processed2012$documents, processed2012$vocab, processed2012$meta)

## check changes in vocabulary
out2012_thresh$vocab
out2012$vocab[!out2012$vocab %in% out2012_thresh$vocab]

## remove discarded observations from data
data2012_thresh <- data2012_thresh[-processed2012$docs.removed,]
data2012_thresh <- data2012_thresh[-out2012_thresh$docs.removed,]

## stm fit with 20 topics 
# (computing sophistication components w/ large number of topics is computationally intractable w/ infrequent terms)
stm_fit2012_thresh <- stm(out2012_thresh$documents, out2012_thresh$vocab
                          , prevalence = as.matrix(out2012_thresh$meta)
                          , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit2012_thresh, out2012_thresh)

## compute combined measures
data2012_thresh$polknow_text_mean_thresh <- (know$ntopics + know$distinct + data2012_thresh$ditem)/3

## merge with original data
data2012 <- data2012_thresh %>% 
  dplyr::select(caseid, polknow_text_mean_thresh) %>% 
  full_join(data2012)
rm(data2012_thresh)



### 2016 ANES
gc()

## combine regular survey and open-ended data, remove spanish and empty responses
data2016_thresh <- anes2016 %>% mutate(resp = apply(anes2016spell[,-1],1,paste,collapse=' ')) %>%
  filter(spanish == 0 & wc != 0)

## remove missings on metadata
data2016_thresh <- data2016_thresh[apply(!is.na(data2016_thresh[,meta2016]),1,prod)==1,]

## process for stm
out2016_thresh <- prepDocuments(processed2016$documents, processed2016$vocab, processed2016$meta)

## check changes in vocabulary
out2016_thresh$vocab
out2016$vocab[!out2016$vocab %in% out2016_thresh$vocab]

## remove discarded observations from data
data2016_thresh <- data2016_thresh[-processed2016$docs.removed,]
data2016_thresh <- data2016_thresh[-out2016_thresh$docs.removed,]

## stm fit with 20 topics 
# (computing sophistication components w/ large number of topics is computationally intractable w/ infrequent terms)
stm_fit2016_thresh <- stm(out2016_thresh$documents, out2016_thresh$vocab
                          , prevalence = as.matrix(out2016_thresh$meta)
                          , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit2016_thresh, out2016_thresh)

## compute combined measures
data2016_thresh$polknow_text_mean_thresh <- (know$ntopics + know$distinct + data2016_thresh$ditem)/3

## merge with original data
data2016 <- data2016_thresh %>% 
  dplyr::select(caseid, polknow_text_mean_thresh) %>% 
  full_join(data2016)
rm(data2016_thresh)



### 2015 YouGov
gc()

## combine regular survey and open-ended data, remove spanish and empty responses
data_yg_thresh <- yougov %>% mutate(resp = apply(opend[,-1],1,paste,collapse=' '))

## remove missings on metadata
data_yg_thresh <- data_yg_thresh[apply(!is.na(data_yg_thresh[,meta]),1,prod)==1,]

## process for stm
out_yougov_thresh <- prepDocuments(processed_yougov$documents, processed_yougov$vocab, processed_yougov$meta)

## remove discarded observations from data
data_yg_thresh <- data_yg_thresh[-processed_yougov$docs.removed,]
data_yg_thresh <- data_yg_thresh[-out_yougov_thresh$docs.removed,]

## stm fit with 20 topics 
# (computing sophistication components w/ large number of topics is computationally intractable w/ infrequent terms)
stm_fit_yg_thresh <- stm(out_yougov$documents, out_yougov$vocab, prevalence = as.matrix(out_yougov$meta)
                         , K=20, init.type = "Spectral", seed=12345)

## combine sophistication components with remaining data
know <- sophistication(stm_fit_yg_thresh, out_yougov_thresh)

## compute combined measures
data_yg_thresh$polknow_text_mean_thresh <- (know$ntopics + know$distinct + data_yg_thresh$ditem)/3

## merge with original data
data_yg <- data_yg_thresh %>% 
  dplyr::select(caseid, polknow_text_mean_thresh) %>% 
  full_join(data_yg_thresh)
rm(data_yg_thresh)



#########
# Validation: compare number of topics
#########

p1 <- ggplot(data2012, aes(x=polknow_text_mean, y=polknow_text_mean_ktopic)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") +  ggtitle("2012 ANES") + xlim(0,1) + ylim(0,1) +
  xlab("") + ylab("Reduced Number of Topics\n(k = 20)") +
  annotate("text", x=0.1, y=max(data2012$polknow_text_mean_ktopic, na.rm = T), size=2
           , label = paste0("r = ",round(cor(data2012$polknow_text_mean, data2012$polknow_text_mean_ktopic, use="complete.obs"), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

p2 <- ggplot(data2016, aes(x=polknow_text_mean, y=polknow_text_mean_ktopic)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") + ggtitle("2016 ANES") + xlim(0,1) + ylim(0,1) +
  xlab("") + ylab("") +
  annotate("text", x=.1, y=max(data2016$polknow_text_mean_ktopic, na.rm = T), size=2
           , label = paste0("r = ",round(cor(data2016$polknow_text_mean, data2016$polknow_text_mean_ktopic, use="complete.obs"), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

p3 <- ggplot(data2012, aes(x=polknow_text_mean, y=polknow_text_mean_thresh)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") + xlim(0,1) + ylim(0,1) +
  xlab(paste0("Preferred Specification\n(k = ",stm_fit2012$settings$dim$K,")",collapse = "")) +
  ylab("Reduced Number of Topics\n+ Include Infrequent Terms") +
  annotate("text", x=0.1, y=max(data2012$polknow_text_mean_thresh), size=2
           , label = paste0("r = ",round(cor(data2012$polknow_text_mean, data2012$polknow_text_mean_thresh, use="complete.obs"), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

p4 <- ggplot(data2016, aes(x=polknow_text_mean, y=polknow_text_mean_thresh)) +
  geom_point(alpha=.05) + geom_smooth(method="lm") + xlim(0,1) + ylim(0,1) +
  xlab(paste0("Preferred Specification\n(k = ",stm_fit2016$settings$dim$K,")",collapse = "")) + 
  ylab("") +
  annotate("text", x=.1, y=max(data2016$polknow_text_mean_thresh), size=2
           , label = paste0("r = ",round(cor(data2016$polknow_text_mean, data2016$polknow_text_mean_thresh, use="complete.obs"), 2))) +
  theme_classic(base_size=8) + theme(panel.border = element_rect(fill=NA))

(p0 <- grid.arrange(p1, p2, p3, p4, ncol=2))
ggsave("../fig/pretext_robustness.pdf", p0, width=4, height=4)


